"""Check a relocated, offline runtime from an unrelated working directory."""
import hashlib
from pathlib import Path
import re
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parent.parent
version = re.search(r"^version:\s*(\S+)", (root / "connector.cabal").read_text(), re.M).group(1)
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
    assert (relocated / "VERSION").read_text().strip() == version
    assert ':protocol 1' in lines[0] and f':version "{version}"' in lines[0], lines[0]
print("PASS: runtime relocates with matching checksums and serves without Clojure CLI")
