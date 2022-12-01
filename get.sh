. .cookie.sh

YEAR=${YEAR:-2021}
DIR=${DIR:-resources}
URL=https://adventofcode.com/"$YEAR"/day/"$1"/input
FILENAME="$DIR/$YEAR/Day$1.txt"

echo "URL=$URL"
echo "FILENAME=$(realpath $FILENAME)"

curl -S -o "$FILENAME" -b "$AOC_COOKIE"  "$URL"
