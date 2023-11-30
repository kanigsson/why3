import argparse
import os
import os.path
import subprocess
import sys

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

configure_opts = [
    "--enable-relocation",
    "--disable-emacs-compilation",
    "--disable-hypothesis-selection",
    "--disable-js-of-ocaml",
    "--disable-zip",
]


def parseargs():
    parser = argparse.ArgumentParser(
        prog="fsf_build.py",
        description="Script to build why3 for SPARK",
    )
    parser.add_argument(
        "--prefix",
        dest="prefix",
        help="target dir for the install",
    )
    args = parser.parse_args()
    return args


def print_command(args):
    for arg in args:
        print(arg, end=" ")
    print("")
    sys.stdout.flush()


def run(args):
    print_command(args)
    p = subprocess.run(args, shell=True)
    p.check_returncode()


def compute_targetdir(prefix=None):
    if not prefix:
        prefix = "staging"
    return os.path.abspath(prefix)


def main():
    args = parseargs()
    run(["opam", "depext"] + depext)
    run(["opam", "install"] + install)
    run(
        [
            "./configure",
            "--prefix=" + compute_targetdir(args.prefix),
        ]
        + configure_opts
    )
    run(["make"])
    run(["make", "install_spark2014"])

main()
