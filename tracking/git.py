from pathlib import Path
import subprocess


def git_commit_message(directory=None) -> str:
    if directory is None:
        directory = Path.cwd()

    commit_message = subprocess.run(
        "git log -1 --pretty=%B",
        shell=True,
        capture_output=True,
        cwd=directory,
        check=True,
    )

    return commit_message.stdout.decode()


def git_commit_hash(directory=None) -> str:
    if directory is None:
        directory = Path.cwd()

    commit_hash = subprocess.run(
        'git log --format="%H" -n 1',
        shell=True,
        capture_output=True,
        cwd=directory,
        check=True,
    )

    return commit_hash.stdout.decode()
