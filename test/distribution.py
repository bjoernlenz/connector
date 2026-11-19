"""Install both source distributions and run outside either source tree.

Requires prepared Cabal dependencies and DATOMIC_CONNECTOR_CLASSPATH jars.
No connector data-directory override is allowed in the installed checks.
"""
import os
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile

root = Path(__file__).resolve().parent.parent
with tempfile.TemporaryDirectory(prefix="connector-distribution-") as temporary:
    work = Path(temporary).resolve()
    archives = work / "archives"
    subprocess.run(["cabal", "sdist", "all", f"--output-directory={archives}"],
                   cwd=root / "starter", check=True)
    source = work / "source"
    source.mkdir()
    for archive in archives.glob("*.tar.gz"):
        with tarfile.open(archive) as package:
            # Archives are generated from this repository immediately above.
            for member in package.getmembers():
                target = (source / member.name).resolve()
                assert target.is_relative_to(source), member.name
            package.extractall(source)
    connector, = source.glob("connector-[0-9]*")
    starter, = source.glob("connector-starter-[0-9]*")
    (source / "cabal.project").write_text(f"packages: {connector.name} {starter.name}\n")
    bin_dir = work / "bin"
    subprocess.run(["cabal", "install", "exe:datomic-bridge", "exe:starter", "--offline",
                    f"--installdir={bin_dir}", "--install-method=copy", "--overwrite-policy=always"],
                   cwd=source, check=True)
    # Installed programs must use Cabal's installed resources, not either source.
    source.rename(work / "hidden-source")
    env = dict(os.environ)
    # Exclude source directories, including any carried by a development classpath.
    jars = [entry for entry in env["DATOMIC_CONNECTOR_CLASSPATH"].split(os.pathsep)
            if entry.endswith(".jar") or entry.endswith("*")]
    assert jars, "Provide resolved dependency jars"
    env["DATOMIC_CONNECTOR_CLASSPATH"] = os.pathsep.join(jars)
    env.pop("connector_datadir", None)
    env.pop("connector_starter_datadir", None)
    env["DATOMIC_CONNECTOR_CLOJURE"] = "/no-clojure-in-java-only-test"
    subprocess.run([sys.executable, str(root / "test/provision.py"), str(bin_dir / "datomic-bridge")],
                   check=True, cwd=work, env=env)
    storage = str(work / "starter-data")
    starter_bin = str(bin_dir / "starter")
    subprocess.run([starter_bin, "init", storage, "adoption", "notes"], cwd=work, env=env, check=True)
    subprocess.run([starter_bin, "demo", storage, "adoption", "notes"], cwd=work, env=env, check=True)
    # An adoption project uses its own immutable migration files.
    migration = work / "001.edn"
    migration.write_text('[{:db/ident :note/category :db/valueType :db.type/string :db/cardinality :db.cardinality/one}]')
    for _ in range(2):
        subprocess.run([starter_bin, "migrate", storage, "adoption", "notes", str(migration)],
                       cwd=work, env=env, check=True)
print("PASS: source distributions install and serve outside source trees; installed starter adopts and migrates")
