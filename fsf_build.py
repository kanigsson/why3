import os
import os.path
import subprocess
import sys

cwd = os.getcwd()

target_dir = os.path.join(cwd, "staging")

def print_command(args):
    for arg in args:
        print(arg, end=" ")
    print("")
    sys.stdout.flush()


def run(args):
    print_command(args)
    subprocess.run(args, shell=True)


depext = ["zarith", "re", "seq", "why3"]
install = [
    "dune",
    "dune-configurator",
    "menhir",
    "num",
    "ocamlgraph",
    "re",
    "seq",
    "yojson",
    "zarith",
    "sexplib",
    "ppx_sexp_conv",
    "ppx_deriving",
]

run(["opam", "depext"] + depext)
run(["opam", "install"] + install)

run(
    [
        os.path.join(cwd, "configure"),
        "--prefix=" + target_dir,
        "--enable-relocation",
        "--disable-emacs-compilation",
        "--disable-hypothesis-selection",
        "--disable-js-of-ocaml",
        "--disable-zip",
    ]
)
run(["make"])
run(["make", "install_spark2014"])
