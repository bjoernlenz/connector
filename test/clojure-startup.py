"""Separate clean CLI dependency preparation from startup verification.

Usage: python3 test/clojure-startup.py prepare|verify ABSOLUTE-STATE-DIR
No developer Maven repository or project deps.edn is used.
"""
import json
import os
from pathlib import Path
import subprocess
import sys

root = Path(__file__).resolve().parent.parent
mode, directory = sys.argv[1:]
state = Path(directory).resolve()
clojure = os.environ.get("DATOMIC_CONNECTOR_CLOJURE", "clojure")
if mode == "prepare":
    state.mkdir(parents=True, exist_ok=False)
    deps = (root / "deps.edn").read_text().replace(
        '"bridge/src"', json.dumps(str(root / "bridge/src"))).replace(
        '".cache/m2"', json.dumps(str(state / "m2")))
    (state / "deps.generated.edn").write_text(deps)
else:
    assert mode == "verify"
deps = (state / "deps.generated.edn").read_text()
env = dict(os.environ, CLJ_CONFIG=str(state / "config"), CLJ_CACHE=str(state / "cache"))
command = [clojure, "-Srepro", "-Sdeps", deps]

def run(args, **kwargs):
    return subprocess.run(command + args, check=True, cwd=state, env=env,
                          text=True, timeout=300, **kwargs)

if mode == "prepare":
    run(["-P"])
    resolved = run(["-Spath"], capture_output=True).stdout.strip()
    (state / "classpath.txt").write_text(resolved + "\n")
    print("PASS: Clojure resolved pinned dependencies into a new Maven repository")
else:
    # Resolver startup should need no downloads after preparation. JVM proxy
    # settings fail HTTP(S) access promptly; this is not an OS network sandbox.
    env["JAVA_TOOL_OPTIONS"] = env.get("JAVA_TOOL_OPTIONS", "") + (
        " -Dhttp.proxyHost=127.0.0.1 -Dhttp.proxyPort=9"
        " -Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=9")
    args = ["-M", "-m", "connector.main"]
    storage = str(state / "data")
    run(args + ["provision", storage, "clean-cli", "test"])
    result = run(args + ["serve", storage, "clean-cli", "test"],
                 input='{:op :hello}\n{:op :query :query [:find ?e :where [?e :db/ident :write/actor]]}\n',
                 capture_output=True)
    lines = result.stdout.splitlines()
    assert len(lines) == 2 and all(':ok true' in line for line in lines), result
    assert ':protocol 1' in lines[0], result
    print("PASS: prepared Clojure CLI starts and serves with HTTP(S) proxies blocked")
