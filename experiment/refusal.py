"""Probe malformed protocol writes; ensure refusal does not commit a transaction."""
import subprocess
import sys

with subprocess.Popen(
    ["scripts/bridge", "serve", *sys.argv[1:]],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, encoding="utf-8",
) as bridge:
    def request(edn):
        bridge.stdin.write(edn + "\n")
        bridge.stdin.flush()
        response = bridge.stdout.readline().strip()
        assert response, "Bridge closed before responding"
        return response

    count = '{:op :query :query [:find (count ?tx) :where [?tx :db/txInstant]]}'
    before = request(count)
    assert before.startswith('{:ok true,'), before
    for tx in [
        '{:op :transact :tx-data [{:test/key "must-not-exist"}]}',
        '{:op :transact :context nil :tx-data []}',
        '{:op :transact :context {:actor 0 :via 1} :tx-data []}',
        '{:op :transact :context {:actor 1} :tx-data []}',
        '{:op :bootstrap :tx-data []}',
    ]:
        result = request(tx)
        assert result.startswith('{:ok false,'), result
        print(result)
    # Datomic Local represents an absent lookup ref as a map with a nil ID.
    result = request('{:op :pull :selector [:db/id] :eid [:test/key "must-not-exist"]}')
    assert result == '{:ok true, :result {:db/id nil}}', result
    # A Datomic error must leave this same bridge usable too.
    result = request('{:op :query :query 42}')
    assert result.startswith('{:ok false,'), result
    assert request(count) == before, "A rejected write committed a transaction"
    bridge.stdin.close()
    assert bridge.wait() == 0
print("PASS: missing/malformed context and bootstrap refused; no transactions committed")
