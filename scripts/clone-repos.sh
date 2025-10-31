#!/usr/bin/env bash
# clone-repos.sh — Multi-repo / multi-branch cloner with worktree-by-default for @branch
# Portable: Bash ≥3.2 (macOS default), Linux, WSL, Git Bash
#
# Behaviour:
#   • Lines starting with "@<branch>" inherit a "fallback repo":
#       - Initially: the CURRENT repo's remote (the repo that contains repos.list).
#       - After each line: fallback updates to the repo used/implied by that line.
#   • "@<branch>" creates a GIT WORKTREE by default.
#   • Per-line opt-out: add "--no-worktree" or "-n" to clone instead of worktree.
#
# Examples (repos.list):
#   @data-tidy data-tidy                 # uses current repo as fallback (worktree)
#   SATVILab/projr                       # fallback → SATVILab/projr
#   @dev                                 # worktree on SATVILab/projr
#   @dev-miguel                          # worktree on SATVILab/projr
#   SATVILab/Analysis@test               # fallback → SATVILab/Analysis
#   @tweak                               # worktree on SATVILab/Analysis
#   @dev-2                               # worktree on SATVILab/Analysis
#
# Notes:
#   • Clone lines still accept "-a|--all-branches" (single-branch is default).
#   • Worktrees for EXTERNAL repos are anchored via a single base clone at
#     "../<repo-name>-base", created on demand. The CURRENT repo uses the CWD as anchor.

set -e

# --- Prerequisites -----------------------------------------------------------
check_prerequisites() {
  for cmd in git; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
      echo "Error: '$cmd' is required but not found in PATH." >&2
      exit 1
    fi
  done
}

# --- Usage -------------------------------------------------------------------
usage() {
  cat <<'EOF'
Usage: clone-repos.sh [--file <repo-list>] [--help]

Line types in <repo-list>:

  1) Explicit repo (clone):
     repo_spec [target_directory] [-a|--all-branches]
       repo_spec: owner/repo[@ref]  OR  https://host/owner/repo[@ref]

  2) Inherited repo branch (worktree by default):
     @branch [target_directory] [--no-worktree|-n]

Rules:
  • '@branch' inherits a fallback repo:
      - Initially the CURRENT repo's remote (the repo containing repos.list).
      - After each line, the fallback becomes that line's repo (minus @ref).
  • '--no-worktree' / '-n' on '@branch' lines opts out to a clone.
  • Lines are order-sensitive for inheritance.
  • Blank lines and lines whose first non-space char is '#' are ignored.
  • target_directory is relative to the PARENT of the current directory.
  • If target_directory is omitted for '@branch', defaults to './<branch>'.

EOF
}

# --- Normalisation helpers ---------------------------------------------------
normalise_remote_to_https() {
  # Convert a remote URL to https://host/owner/repo (no .git)
  local url="$1" host path
  case "$url" in
    https://*)
      url="${url%.git}"
      printf '%s\n' "$url"
      ;;
    ssh://git@*)
      url="${url#ssh://git@}"
      host="${url%%/*}"
      path="${url#*/}"
      printf 'https://%s/%s\n' "$host" "${path%.git}"
      ;;
    git@*:* )
      host="${url#git@}"; host="${host%%:*}"
      path="${url#*:}";   path="${path%.git}"
      printf 'https://%s/%s\n' "$host" "$path"
      ;;
    *)
      printf '%s\n' "$url"
      ;;
  esac
}

spec_to_https() {
  # owner/repo → https://github.com/owner/repo ; https stays https
  local spec="$1"
  case "$spec" in
    https://*) printf '%s\n' "${spec%.git}" ;;
    */*)       printf 'https://github.com/%s\n' "${spec%.git}" ;;
    *)         printf '%s\n' "$spec" ;;
  esac
}

repo_basename_from_https() {
  local url="$1"
  url="${url%/}"
  printf '%s\n' "${url##*/}"
}

# --- Git helpers -------------------------------------------------------------
get_current_repo_remote_https() {
  if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "Error: not inside a Git working tree; cannot derive fallback repo." >&2
    exit 1
  fi
  local url=""
  if git remote | grep -qx 'origin'; then
    url="$(git remote get-url --push origin 2>/dev/null || git remote get-url origin 2>/dev/null || true)"
  fi
  if [ -z "$url" ]; then
    local first
    first="$(git remote 2>/dev/null | head -n1 || true)"
    if [ -n "$first" ]; then
      url="$(git remote get-url --push "$first" 2>/dev/null || git remote get-url "$first" 2>/dev/null || true)"
    fi
  fi
  if [ -z "$url" ]; then
    echo "Error: no Git remotes found in the current repository." >&2
    exit 1
  fi
  normalise_remote_to_https "$url"
}

default_remote_branch() {
  local base="$1" ref
  ref="$(git -C "$base" symbolic-ref -q --short refs/remotes/origin/HEAD 2>/dev/null || true)"
  [ -n "$ref" ] && printf '%s\n' "${ref#origin/}" || printf 'main\n'
}

local_branch_exists()   { git -C "$1" rev-parse --verify --quiet "refs/heads/$2" >/dev/null; }
remote_branch_exists()  { git -C "$1" rev-parse --verify --quiet "refs/remotes/origin/$2" >/dev/null; }

find_worktree_for_branch() {
  local base="$1" branch="$2" line
  line="$(git -C "$base" worktree list 2>/dev/null | grep " \[$branch\]" | head -n1 || true)"
  [ -n "$line" ] && printf '%s\n' "${line%% *}"
}

# --- Base clone management for worktrees -------------------------------------
ensure_base_clone_for_repo() {
  # For CURRENT repo, use CWD; for external, ensure ../<name>-base exists.
  local repo_https="$1" current_https="$2" current_path="$3" parent_dir="$4"
  if [ "$repo_https" = "$current_https" ]; then
    printf '%s\n' "$current_path"
    return 0
  fi
  local name base
  name="$(repo_basename_from_https "$repo_https")"
  base="$parent_dir/${name}-base"
  if [ -d "$base/.git" ]; then
    printf '%s\n' "$base"; return 0
  fi
  mkdir -p "$base"
  if [ -n "$(ls -A "$base" 2>/dev/null)" ]; then
    echo "Error: base anchor '$base' exists and is not empty." >&2
    return 1
  fi
  echo "Creating base clone for $repo_https → $base"
  git clone "$repo_https" "$base"
  printf '%s\n' "$base"
}

# --- Parsing -----------------------------------------------------------------
# Returns: repo_spec \x1f target_dir \x1f all_branches \x1f is_worktree
parse_effective_line() {
  set -f
  local line="$1" fallback_repo_https="$2"
  local first target_dir="" all_branches=0 is_worktree=0 no_worktree=0

  set -- $line
  [ "$#" -eq 0 ] && { printf '%s\x1f%s\x1f%s\x1f%s\n' "" "" "0" "0"; set +f; return 0; }

  first="$1"; shift
  case "$first" in
    @*)
      local branch="${first#@}"
      while [ "$#" -gt 0 ]; do
        case "$1" in
          -n|--no-worktree) no_worktree=1 ;;
          -a|--all-branches) all_branches=1 ;;  # harmless for worktrees
          -*)
            echo "Warning: ignoring unknown option '$1' on line: $line" >&2 ;;
          *)
            if [ -z "$target_dir" ]; then target_dir="$1"
            else echo "Error: multiple target directories on one line: $line" >&2; set +f; return 1
            fi ;;
        esac
        shift
      done
      [ -z "$fallback_repo_https" ] && { echo "Error: no fallback repo available for '$line'."; set +f; return 1; }
      is_worktree=$(( no_worktree ? 0 : 1 ))
      printf '%s@%s\x1f%s\x1f%s\x1f%s\n' "$fallback_repo_https" "$branch" "$target_dir" "$all_branches" "$is_worktree"
      ;;
    *)
      local repo_spec="$first"
      while [ "$#" -gt 0 ]; do
        case "$1" in
          -a|--all-branches) all_branches=1 ;;
          -n|--no-worktree)  echo "Warning: '--no-worktree' ignored on clone line: $line" >&2 ;;
          -*)
            echo "Warning: ignoring unknown option '$1' on line: $line" >&2 ;;
          *)
            if [ -z "$target_dir" ]; then target_dir="$1"
            else echo "Error: multiple target directories on one line: $line" >&2; set +f; return 1
            fi ;;
        esac
        shift
      done
      is_worktree=0
      printf '%s\x1f%s\x1f%s\x1f%s\n' "$repo_spec" "$target_dir" "$all_branches" "$is_worktree"
      ;;
  esac
  set +f
}

# --- Clone flow --------------------------------------------------------------
clone_one_repo() {
  local repo_spec="$1" target_dir="$2" base_dir="$3" all_branches="$4"

  local repo_url_no_ref ref
  case "$repo_spec" in
    *@*) repo_url_no_ref="${repo_spec%@*}"; ref="${repo_spec##*@}" ;;
    *)   repo_url_no_ref="$repo_spec"; ref="" ;;
  esac

  local repo_url repo_dir
  case "$repo_url_no_ref" in
    https://*)
      repo_url="$repo_url_no_ref"
      repo_dir=$(basename "${repo_url_no_ref%.git}")
      ;;
    */*)
      repo_url="https://github.com/$repo_url_no_ref"
      repo_dir="${repo_url_no_ref#*/}"
      ;;
    *)
      echo "Error: invalid repo spec '$repo_spec'." >&2; return 1 ;;
  esac

  local dest
  if [ -n "$target_dir" ]; then dest="$base_dir/$target_dir"
  else dest="$base_dir/$repo_dir"; fi

  if [ -d "$dest/.git" ]; then
    echo "Already exists: $dest"; return 0
  fi

  mkdir -p "$dest"
  if [ -n "$(ls -A "$dest" 2>/dev/null)" ]; then
    echo "Error: destination '$dest' exists and is not empty." >&2; return 1
  fi

  local clone_opts=()
  if [ "${all_branches:-0}" -eq 0 ]; then clone_opts=(--single-branch); fi
  [ -n "$ref" ] && clone_opts=("${clone_opts[@]}" --branch "$ref")

  echo "Cloning $repo_url → $dest ${ref:+(branch $ref)}"
  git clone "${clone_opts[@]}" "$repo_url" "$dest"
}

# --- Worktree flow -----------------------------------------------------------
create_worktree_for_branch() {
  # Args: repo_https branch target_dir parent_dir current_https current_path
  local repo_https="$1" branch="$2" target_dir="$3" parent_dir="$4" current_https="$5" current_path="$6"
  [ -z "$branch" ] && { echo "Error: @branch requires a branch name."; return 1; }

  local dest="$parent_dir/${target_dir:-$branch}"
  local base
  base="$(ensure_base_clone_for_repo "$repo_https" "$current_https" "$current_path" "$parent_dir")" || return 1

  if [ -d "$dest/.git" ]; then
    if git -C "$dest" rev-parse --abbrev-ref HEAD >/dev/null 2>&1; then
      local curb; curb="$(git -C "$dest" rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
      if [ "$curb" = "$branch" ]; then
        echo "Already exists: $dest (branch $branch)"
        return 0
      fi
    fi
    echo "Error: destination '$dest' already exists and is not the expected worktree." >&2
    return 1
  fi

  git -C "$base" fetch --prune origin

  local existing; existing="$(find_worktree_for_branch "$base" "$branch" || true)"
  if [ -n "$existing" ]; then
    echo "Branch '$branch' already checked out at: $existing"
    return 0
  fi

  mkdir -p "$dest"
  if [ -n "$(ls -A "$dest" 2>/dev/null)" ]; then
    echo "Error: destination '$dest' exists and is not empty." >&2
    return 1
  fi

  if local_branch_exists "$base" "$branch"; then
    echo "Adding worktree $dest (existing local branch '$branch')"
    git -C "$base" worktree add "$dest" "$branch"
  elif remote_branch_exists "$base" "$branch"; then
    echo "Adding worktree $dest (tracking origin/$branch)"
    git -C "$base" worktree add -b "$branch" "$dest" "origin/$branch"
    git -C "$dest" branch --set-upstream-to "origin/$branch" || true
  else
    local defb; defb="$(default_remote_branch "$base")"
    echo "Adding worktree $dest (new branch '$branch' from origin/$defb)"
    git -C "$base" worktree add -b "$branch" "$dest" "origin/$defb"
    git -C "$dest" branch --set-upstream-to "origin/$branch" >/dev/null 2>&1 || true
  fi
}

# --- Argument parsing --------------------------------------------------------
parse_args() {
  if [ ! -f "repos.list" ] && [ -f "repos-to-clone.list" ]; then
    REPOS_FILE="repos-to-clone.list"
  else
    REPOS_FILE="repos.list"
  fi

  while [ "$#" -gt 0 ]; do
    case "$1" in
      -f|--file) shift; [ "$#" -gt 0 ] && REPOS_FILE="$1" || { usage; exit 1; }; shift ;;
      -h|--help) usage; exit 0 ;;
      *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
    esac
  done

  if [ ! -f "$REPOS_FILE" ]; then
    echo "File '$REPOS_FILE' not found." >&2
    exit 1
  fi
}

# --- Main --------------------------------------------------------------------
main() {
  check_prerequisites
  parse_args "$@"

  local start_dir parent_dir
  start_dir="$(pwd)"
  parent_dir="$(dirname "$start_dir")"

  # Fallback repo initialised to the CURRENT repo's remote
  local current_repo_https fallback_repo_https
  current_repo_https="$(get_current_repo_remote_https)"
  fallback_repo_https="$current_repo_https"

  while IFS= read -r line || [ -n "$line" ]; do
    # Trim and strip comments
    local trimmed="${line#"${line%%[![:space:]]*}"}"
    case "$trimmed" in
      \#*|"") continue ;;
    esac
    # Strip inline comments starting with ' #'
    case "$trimmed" in
      *" # "*) trimmed="${trimmed%% # *}" ;;
      *" #"*)  trimmed="${trimmed%% #*}"  ;;
    esac
    trimmed="${trimmed%"${trimmed##*[![:space:]]}"}"
    trimmed=${trimmed%$'\r'}
    [ -z "$trimmed" ] && continue

    # Parse with current fallback
    local parsed repo_spec target_dir all_branches is_worktree
    if ! parsed="$(parse_effective_line "$trimmed" "$fallback_repo_https")"; then
      echo "Failed to parse line: $line" >&2
      exit 1
    fi
    IFS=$'\x1f' read -r repo_spec target_dir all_branches is_worktree <<<"$parsed"
    [ -z "$repo_spec" ] && continue

    # Perform action
    if [ "$is_worktree" -eq 1 ]; then
      local branch=""
      case "$repo_spec" in
        *@*) branch="${repo_spec##*@}" ;;
        *)   echo "Error: internal parse error (missing @branch for worktree)." >&2; exit 1 ;;
      esac
      # Determine the repo (strip @ref) in https form
      local repo_https
      repo_https="$(spec_to_https "${repo_spec%@*}")"
      create_worktree_for_branch "$repo_https" "$branch" "$target_dir" "$parent_dir" "$current_repo_https" "$start_dir"
      # Update fallback to this repo
      fallback_repo_https="$repo_https"
    else
      clone_one_repo "$repo_spec" "$target_dir" "$parent_dir" "$all_branches"
      # Update fallback to this repo (strip @ref)
      case "$repo_spec" in
        *@*) fallback_repo_https="$(spec_to_https "${repo_spec%@*}")" ;;
        *)   fallback_repo_https="$(spec_to_https "$repo_spec")" ;;
      esac
    fi

  done <"$REPOS_FILE"
}

main "$@"
