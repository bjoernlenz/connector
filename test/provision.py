"""Exercise generic provisioning through either installed CLI or bundled runtime."""
from pathlib import Path
import fcntl
import re
import subprocess
import sys
import tempfile

bridge = str(Path(sys.argv[1]).resolve())
with tempfile.TemporaryDirectory(prefix="connector-provision-") as temporary:
    work = Path(temporary)
    storage = str(work / "data")

    def run(mode, database, files=(), request=None, success=True):
        result = subprocess.run([bridge, mode, storage, "project", database, *files],
                                input=request, text=True, capture_output=True, cwd=work, timeout=60)
        assert (result.returncode == 0) == success, result
        return result.stdout if success else result.stderr

    (work / "invalid.edn").write_text("{}")
    (work / "schema.edn").write_text('''[{:db/ident :app/name :db/valueType :db.type/string
                                       :db/cardinality :db.cardinality/one}]''')
    (work / "identities.edn").write_text('''[{:db/ident :identity/alice :app/name "Alice"}
                                           {:db/ident :identity/project :app/name "Project"}]''')
    # Parsing failure must precede database creation, so the same name remains usable.
    assert "transaction vectors" in run("provision", "custom", ["invalid.edn"], success=False)
    result = run("provision", "custom", ["schema.edn", "identities.edn"])
    assert ':initial-transactions 3' in result, result
    checks = '''{:op :query :query [:find ?name :where [_ :app/name ?name]]}
{:op :query :query [:find ?e :where [?e :db/ident :test/key]]}
{:op :query :query [:find ?e :where [?e :db/ident :identity/bjoern]]}
{:op :pull :selector [:db/ident :db/valueType] :eid :write/actor}
{:op :query :query [:find (count ?tx) :where [?tx :db/txInstant]]}
'''
    invalid = run("serve", "custom", request='{:op :pull :snapshot "missing" :selector [:db/id] :eid :write/actor}\n' + checks).splitlines()
    assert ':ok false' in invalid[0] and 'Unknown or released snapshot' in invalid[0], invalid
    assert all(line.startswith("{:ok true,") for line in invalid[1:]), invalid
    with subprocess.Popen([bridge, "serve", storage, "project", "custom"],
                          stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, cwd=work) as process:
        def exchange(request):
            process.stdin.write(request + "\n")
            process.stdin.flush()
            return process.stdout.readline()
        for selector in ('nil', '{}', '{:t -1}', '{:t 9223372036854775808N}',
                         '{:t 999999999}', '{:instant "not-a-date"}',
                         '{:instant #inst "2026-01-01"}',
                         '{:instant 9223372036854775808N}', '{:t 0 :instant 0}'):
            rejected = exchange('{:op :snapshot/open :as-of ' + selector + '}')
            assert ':ok false' in rejected, (selector, rejected)
            assert ':ok true' in exchange('{:op :hello}'), "Rejection poisoned bridge"
        for selector in ('{:t 0}', '{:instant 0}'):
            historical = exchange('{:op :snapshot/open :as-of ' + selector + '}')
            assert ':ok true' in historical and ':as-of ' in historical, historical
            token = re.search(r':snapshot "([^"\n]+)"', historical).group(1)
            empty = exchange('{:op :query :snapshot "' + token + '" :query [:find ?e :where [?e :db/ident :write/actor]]}')
            assert ':result []' in empty, empty
            assert ':ok true' in exchange('{:op :snapshot/close :snapshot "' + token + '"}')
        opened = exchange('{:op :snapshot/open}')
        token = re.search(r':snapshot "([^"\n]+)"', opened).group(1)
        assert ':ok true' in exchange('{:op :snapshot/close :snapshot "' + token + '"}')
        assert 'Unknown or released snapshot' in exchange('{:op :pull :selector [:db/id] :eid :write/actor :snapshot "' + token + '"}')
        assert ':ok true' in exchange('{:op :pull :selector [:db/id] :eid :write/actor}')
    before = run("serve", "custom", request=checks).splitlines()
    assert all(line.startswith("{:ok true,") for line in before), before
    assert '"Alice"' in before[0] and '"Project"' in before[0], before
    assert before[1:3] == ['{:ok true, :result []}'] * 2, before
    assert ':db/ident :write/actor' in before[3] and ':db.type/ref' in before[3], before
    assert "already exists" in run("provision", "custom", ["schema.edn"], success=False)
    after = run("serve", "custom", request=checks).splitlines()
    assert after[-1] == before[-1], "Reprovisioning committed a transaction"
    with (Path(storage) / ".connector-provision.lock").open("a") as held:
        fcntl.lockf(held, fcntl.LOCK_EX | fcntl.LOCK_NB)
        assert "already in progress" in run("provision", "core-only", success=False)
    assert ':initial-transactions 1' in run("provision", "core-only")
    core = run("serve", "core-only", request='''{:op :pull :selector [:db/ident] :eid :write/via}
{:op :query :query [:find ?e :where [?e :db/ident :identity/alice]]}
''').splitlines()
    assert ':db/ident :write/via' in core[0] and core[1] == '{:ok true, :result []}', core
print("PASS: project-owned provisioning, core-only setup, preflight, locking and existing-database refusal")
