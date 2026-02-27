#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded
oops() {
    echo "$0:" "$@" >&2
    exit 1
}

umask 0022

tmpDir="$(mktemp -d -t nix-binary-tarball-unpack.XXXXXXXXXX || \
          oops "Can't create temporary directory for downloading the Nix binary tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

require_util() {
    command -v "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

case "$(uname -s).$(uname -m)" in
    Linux.x86_64)
        hash=96c5035a63699a12301566fca7838bb1264c3b0894dc0f610101726753e52ac2
        path=crkzp9q2agkinm7ma959j7bk98ipf88p/nix-2.33.3-x86_64-linux.tar.xz
        system=x86_64-linux
        ;;
    Linux.i?86)
        hash=6756f32a3b68c3aabd09cb025fc5929b7616ca41fbca07dc70e48db83e41f662
        path=b2gl94b6nvb7dsishycr1g01ax0s1955/nix-2.33.3-i686-linux.tar.xz
        system=i686-linux
        ;;
    Linux.aarch64)
        hash=15e3a7fca83b28170be0e6a1dff2c6645529f899c76e789575c8ce9b3251bb2d
        path=xk03q4ykfd2ggmmw74vk2yg8ph8w04d4/nix-2.33.3-aarch64-linux.tar.xz
        system=aarch64-linux
        ;;
    Linux.armv6l)
        hash=da025b51446c835c394691fcaf9ff6992975bf394b290c09c678fb8ff5434d7b
        path=fd31kwy1fjx73b13s950ly15ag0z87yc/nix-2.33.3-armv6l-linux.tar.xz
        system=armv6l-linux
        ;;
    Linux.armv7l)
        hash=751b1f63cb186f1152bf157aba39c72e65f87b9816f13706d2287b52de33c4e8
        path=8pbpyy1r6xxwbgrdawmljbzx26171ivn/nix-2.33.3-armv7l-linux.tar.xz
        system=armv7l-linux
        ;;
    Linux.riscv64)
        hash=651c0712733543405b675275835b345833e838cc92c862e643c7596605254a17
        path=zc7sq40cdl9srdqvar0gx90zkni8w6zb/nix-2.33.3-riscv64-linux.tar.xz
        system=riscv64-linux
        ;;
    Darwin.x86_64)
        hash=fbadda2f407d73409b444ab21386b5f713947e474ee4f4f96a9cbc6ccd9f853b
        path=365v0j8idldbz1dxcx4gwcrx4bci8cg7/nix-2.33.3-x86_64-darwin.tar.xz
        system=x86_64-darwin
        ;;
    Darwin.arm64|Darwin.aarch64)
        hash=0781837b43d0234933141f6305c8a8fec6d49f3d2b245ee5b7c4e5d2a4a39c38
        path=8cjgja44sy3plhsa2n43gqsm7zgbh7my/nix-2.33.3-aarch64-darwin.tar.xz
        system=aarch64-darwin
        ;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

# Use this command-line option to fetch the tarballs using nar-serve or Cachix
if [ "${1:-}" = "--tarball-url-prefix" ]; then
    if [ -z "${2:-}" ]; then
        oops "missing argument for --tarball-url-prefix"
    fi
    url=${2}/${path}
    shift 2
else
    url=https://releases.nixos.org/nix/nix-2.33.3/nix-2.33.3-$system.tar.xz
fi

tarball=$tmpDir/nix-2.33.3-$system.tar.xz

require_util tar "unpack the binary tarball"
if [ "$(uname -s)" != "Darwin" ]; then
    require_util xz "unpack the binary tarball"
fi

if command -v curl > /dev/null 2>&1; then
    fetch() { curl --fail -L "$1" -o "$2"; }
elif command -v wget > /dev/null 2>&1; then
    fetch() { wget "$1" -O "$2"; }
else
    oops "you don't have wget or curl installed, which I need to download the binary tarball"
fi

echo "downloading Nix 2.33.3 binary tarball for $system from '$url' to '$tmpDir'..."
fetch "$url" "$tarball" || oops "failed to download '$url'"

if command -v sha256sum > /dev/null 2>&1; then
    hash2="$(sha256sum -b "$tarball" | cut -c1-64)"
elif command -v shasum > /dev/null 2>&1; then
    hash2="$(shasum -a 256 -b "$tarball" | cut -c1-64)"
elif command -v openssl > /dev/null 2>&1; then
    hash2="$(openssl dgst -r -sha256 "$tarball" | cut -c1-64)"
else
    oops "cannot verify the SHA-256 hash of '$url'; you need one of 'shasum', 'sha256sum', or 'openssl'"
fi

if [ "$hash" != "$hash2" ]; then
    oops "SHA-256 hash mismatch in '$url'; expected $hash, got $hash2"
fi

unpack=$tmpDir/unpack
mkdir -p "$unpack"
tar -xJf "$tarball" -C "$unpack" || oops "failed to unpack '$url'"

script=$(echo "$unpack"/*/install)

[ -e "$script" ] || oops "installation script is missing from the binary tarball!"
export INVOKED_FROM_INSTALL_IN=1
"$script" "$@"

} # End of wrapping
