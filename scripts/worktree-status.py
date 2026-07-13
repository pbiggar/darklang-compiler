#!/usr/bin/env python3
"""Show worktree status with merge info relative to main."""

import subprocess
import time
import sys
import os
import select

# Colors (same as original)
GREEN = '\033[0;32m'
YELLOW = '\033[0;33m'
RED = '\033[0;31m'
CYAN = '\033[0;36m'
DIM = '\033[0;90m'
BOLD = '\033[1m'
NC = '\033[0m'

REFRESH_INTERVAL = 2

def run_git(args, cwd=None, check=False):
    """Run git command and return stdout."""
    safe_cwd = cwd if cwd else None
    try:
        result = subprocess.run(['git'] + args, cwd=safe_cwd, capture_output=True, text=True)
    except OSError as e:
        if check:
            raise RuntimeError(f"Failed to run git {' '.join(args)}: {e}") from e
        return ""

    if result.returncode != 0:
        if check:
            stderr = (result.stderr or "").strip()
            stdout = (result.stdout or "").strip()
            details = stderr if stderr else (stdout if stdout else f"exit code {result.returncode}")
            raise RuntimeError(f"git {' '.join(args)} failed: {details}")
        return ""

    return result.stdout.strip()


def resolve_repo_root():
    """Resolve git repo root from the current working directory."""
    return run_git(['rev-parse', '--show-toplevel'], check=True)

def format_age(seconds):
    """Format age in human-readable form."""
    if seconds < 60: return f"{int(seconds)}s"
    if seconds < 3600: return f"{int(seconds // 60)}m"
    if seconds < 86400: return f"{int(seconds // 3600)}h"
    if seconds < 604800: return f"{int(seconds // 86400)}d"
    if seconds < 2592000: return f"{int(seconds // 604800)}w"
    if seconds < 31536000: return f"{int(seconds // 2592000)}mo"
    return f"{int(seconds // 31536000)}y"

def get_worktree_status_flags(path):
    """Get staged/unstaged/untracked flags."""
    porcelain = run_git(['status', '--porcelain=1'], cwd=path)
    staged = CYAN + 's' + NC if any(l[0] in 'MADRCU' for l in porcelain.split('\n') if l) else ' '
    unstaged = CYAN + 'u' + NC if any(l[1:2] in 'MADRCU' for l in porcelain.split('\n') if l) else ' '
    untracked = CYAN + '?' + NC if any(l.startswith('??') for l in porcelain.split('\n') if l) else ' '
    return staged + unstaged + untracked

def get_unpushed_count(path):
    """Get count of unpushed commits."""
    upstream = run_git(['rev-parse', '--abbrev-ref', '--symbolic-full-name', '@{u}'], cwd=path)
    if not upstream: return ""
    count = run_git(['rev-list', '--count', f'{upstream}..HEAD'], cwd=path)
    return f" {RED}↑ {count} unpushed{NC}" if count and count != "0" else ""

def get_log_entries(repo_root, count=11):
    """Get recent log entries."""
    output = run_git(['log', f'-{count}', '--pretty=format:%ct\x1f%h\x1f%s\x1f%b\x1e'], cwd=repo_root)
    entries = []
    now = time.time()
    for rec in output.split('\x1e'):
        if not rec.strip(): continue
        parts = rec.split('\x1f')
        if len(parts) < 3: continue
        try:
            age_sec = max(0, now - float(parts[0].strip()))
        except ValueError:
            age_sec = 0
        entries.append({
            'age': format_age(age_sec),
            'hash': parts[1].strip(),
            'subject': parts[2].replace('\n', ' ').strip()[:62],
            'body': parts[3].replace('\n', ' ').strip() if len(parts) > 3 else ""
        })
    return entries

def get_worktrees(repo_root):
    """Get list of worktrees with status info."""
    if not repo_root:
        return []
    try:
        output = subprocess.run(
            ['git', 'worktree', 'list'],
            cwd=repo_root,
            capture_output=True,
            text=True
        ).stdout
    except OSError:
        return []
    worktrees = []
    for line in output.strip().split('\n'):
        if not line: continue
        parts = line.split()
        path = parts[0]
        prunable = 'prunable' in line
        # Extract branch name
        branch = line.split('[')[-1].rstrip(']').replace(' prunable', '') if '[' in line else ''

        # Short display name
        display = branch
        if display.startswith('compiler-for-dark-'):
            display = display[18:]
        elif display.startswith('compiler-for-dark'):
            display = display[17:].lstrip('-')

        worktrees.append({'path': path, 'branch': branch, 'display': display, 'prunable': prunable})
    return worktrees

def vislen(s):
    """Get visible length of string (excluding ANSI codes).

    Matches original bash script's awk vislen function which normalizes
    unicode characters to single-width ASCII for consistent alignment.
    """
    import re
    # Remove ANSI escape codes
    clean = re.sub(r'\033\[[0-9;]*m', '', s)
    # Normalize unicode arrows/checkmark to single-char ASCII (matches original awk)
    clean = clean.replace('↑', '^').replace('↓', 'v').replace('✓', 'v')
    return len(clean)

def truncate_plain_label(label, max_width):
    """Truncate non-colored labels to max visible width."""
    if vislen(label) <= max_width:
        return label
    return label[:max_width]

def render_status():
    """Render the full status display."""
    repo_root = resolve_repo_root()
    try:
        term_cols = os.get_terminal_size().columns if sys.stdout.isatty() else 120
    except OSError:
        term_cols = 120

    worktrees = get_worktrees(repo_root)
    log_entries = get_log_entries(repo_root)
    context = time.strftime('%H:%M:%S')
    max_rows = 11
    sorted_worktrees = sorted(worktrees, key=lambda w: (0 if w['branch'] == 'main' else 1, w['branch']))
    visible_worktrees = sorted_worktrees[:max_rows - 1]

    # Build rows with worktree info
    rows = []
    for wt in visible_worktrees:
        path, branch, display = wt['path'], wt['branch'], wt['display']
        prunable = f" {DIM}(prunable){NC}" if wt['prunable'] else ""

        if not os.path.isdir(path):
            rows.append({'label': display, 'flags': '   ', 'status': f"{DIM}(inaccessible){NC}{prunable}"})
            continue

        # Check if git repo is accessible
        if run_git(['rev-parse', 'HEAD'], cwd=path) == "":
            rows.append({'label': display, 'flags': '   ', 'status': f"{DIM}(inaccessible){NC}{prunable}"})
            continue

        flags = get_worktree_status_flags(path)

        if branch == 'main':
            unpushed = get_unpushed_count(path)
            rows.append({'label': f"{CYAN}main{NC}", 'flags': flags, 'status': f"{GREEN}✓{NC}{unpushed}{prunable}"})
        else:
            ahead = run_git(['rev-list', '--count', 'main..HEAD'], cwd=path) or "?"
            behind = run_git(['rev-list', '--count', 'HEAD..main'], cwd=path) or "?"
            if ahead == "0" and behind == "0":
                status = f"{GREEN}✓{NC}"
            elif ahead == "0":
                status = f"{YELLOW}↓{behind}{NC}"
            elif behind == "0":
                status = f"{RED}↑{ahead}{NC}"
            else:
                status = f"{RED}↑{ahead} ↓{behind}{NC}"
            rows.append({'label': display, 'flags': flags, 'status': status + prunable})

    # Calculate max widths
    max_branch = min(8, max((vislen(r['label']) for r in rows), default=4))
    max_status = max((vislen(r['status']) for r in rows), default=1)
    max_log_age = max((len(e['age']) for e in log_entries), default=2)

    # Output (11 rows total: worktrees + context row + remaining log entries)
    output_lines = []
    log_idx = 0
    max_visible = min(len(rows), max_rows - 1)

    def build_log_display(entry):
        if not entry:
            return ""
        age_pad = max_log_age - len(entry['age'])
        subject = entry['subject']
        body = entry['body']

        # Calculate space available for log portion (after worktree columns)
        # Columns: label(max_branch) + sep(3) + flags(3) + sep(2) + status(max_status) + sep(2)
        worktree_cols = max_branch + 3 + 3 + 2 + max_status + 2
        # Log portion: age(max_log_age) + space(1) + hash(7) + space(1) + subject + optional(space + body)
        log_prefix = max_log_age + 1 + len(entry['hash']) + 1
        available_for_text = term_cols - worktree_cols - log_prefix

        if available_for_text <= 0:
            # No room for subject/body
            return f"{DIM}{entry['age']}{NC}{' ' * age_pad} {DIM}{entry['hash']}{NC}"

        # Truncate subject if needed
        if len(subject) > available_for_text:
            subject = subject[:available_for_text]
            body = ""
        else:
            # Space remaining after subject for body
            remaining = available_for_text - len(subject) - 1  # -1 for space before body
            if remaining > 0 and body:
                body = body[:remaining]
            else:
                body = ""

        if body:
            return f"{DIM}{entry['age']}{NC}{' ' * age_pad} {DIM}{entry['hash']}{NC} {subject}{DIM} {body}{NC}"
        else:
            return f"{DIM}{entry['age']}{NC}{' ' * age_pad} {DIM}{entry['hash']}{NC} {subject}"

    for i in range(1, max_rows + 1):
        if i == max_visible + 1:
            # Context row
            log = log_entries[log_idx] if log_idx < len(log_entries) else None
            log_idx += 1
            log_str = build_log_display(log)
            # Keep full HH:MM:SS label and backfill remaining worktree columns with spaces.
            worktree_prefix_width = max_branch + 3 + 3 + 2 + max_status + 2
            pad_context = worktree_prefix_width - vislen(context)
            if pad_context < 0:
                pad_context = 0
            output_lines.append(f"{DIM}{context}{NC}{' ' * pad_context}{log_str}")
        else:
            data_index = (i - 1) if i <= max_visible else (i - 2)
            if data_index < len(rows):
                r = rows[data_index]
                label = r['label']
                # Truncate label if too long (but preserve color codes)
                if vislen(label) > max_branch and '\033' not in label:
                    label = truncate_plain_label(label, max_branch)
                flags = r['flags']
                status = r['status']
            else:
                label = ""
                flags = "   "
                status = ""

            log = log_entries[log_idx] if log_idx < len(log_entries) else None
            log_idx += 1
            log_str = build_log_display(log)

            pad_branch = max_branch - vislen(label)
            if pad_branch < 0:
                pad_branch = 0
            pad_status = max_status - vislen(status)
            if pad_status < 0:
                pad_status = 0

            output_lines.append(f"{label}{' ' * pad_branch}   {flags}  {status}{' ' * pad_status}  {log_str}")

    return '\n'.join(output_lines)

def main(once=False):
    if once:
        # Print once and exit
        print(render_status())
        return

    # Enter alternate screen, hide cursor
    print('\033[?1049h\033[22;0;0t\033[?25l', end='', flush=True)
    try:
        while True:
            output = render_status()
            # In raw mode, \n doesn't include carriage return, so use \r\n
            output = output.replace('\n', '\r\n')
            print(f'\033[H\033[J{output}', end='\r\n', flush=True)
            # Check for 'q' or ESC with timeout
            if sys.stdin in select.select([sys.stdin], [], [], REFRESH_INTERVAL)[0]:
                key = sys.stdin.read(1)
                if key in ('q', '\x1b'):
                    break
    finally:
        # Restore screen, show cursor
        print('\033[?1049l\033[23;0;0t\033[?12l\033[?25h', end='', flush=True)

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Show worktree status with merge info')
    parser.add_argument('-1', '--once', action='store_true', help='Print once and exit')
    args = parser.parse_args()

    if args.once:
        main(once=True)
    else:
        import tty, termios
        fd = sys.stdin.fileno()
        old = termios.tcgetattr(fd)
        try:
            tty.setraw(fd)
            main()
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old)
