"""Check a relocated, offline runtime from an unrelated working directory."""
import hashlib
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent.parent
with tempfile.TemporaryDirectory(prefix="connector-runtime-") as temporary:
    work = Path(temporary)
    built = work / "built"
    subprocess.run([str(root / "scripts/package-runtime"), str(built)], check=True, cwd=work)
    relocated = work / "relocated runtime"
    built.rename(relocated)
    for line in (relocated / "SHA256SUMS").read_text().splitlines():
        digest, path = line.split("  ", 1)
        assert hashlib.sha256((relocated / path).read_bytes()).hexdigest() == digest, path
    subprocess.run([sys.executable, str(root / "test/provision.py"), str(relocated / "run")], check=True, cwd=work)
    result = subprocess.run(
        [str(relocated / "run"), "serve", *sys.argv[1:]],
        input='{:op :hello}\n{:op :query :query [:find (count ?tx) :where [?tx :db/txInstant]]}\n',
        text=True, capture_output=True, check=True, cwd=work, timeout=60)
    lines = result.stdout.splitlines()
    assert len(lines) == 2 and all(line.startswith("{:ok true,") for line in lines), result
    assert ':protocol 1' in lines[0] and ':version "0.1.0.0"' in lines[0], lines[0]
print("PASS: runtime relocates with matching checksums and serves without Clojure CLI")
