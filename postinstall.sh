set -u
set -e
set -o pipefail

OCAMLRUN=$(node -p 'require.resolve("@esy-ocaml/ocamlrun/install/bin/ocamlrun")')
FASTREPLACESTRING=$(node -p 'require.resolve("fastreplacestring/.bin/fastreplacestring.exe")')

echo "#!$OCAMLRUN" > ./esyb
cat esyb.bc >> ./esyb
chmod +x ./esyb

ln -s "$FASTREPLACESTRING" ./fastreplacestring.exe
