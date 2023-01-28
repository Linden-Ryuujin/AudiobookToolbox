
function PrintHelp {
    echo "chapters.sh ACTION [ACTION PARAMTERS]"
    echo
    echo Actions:
    echo   "    -a, --add       [chapters.txt] [audiobook.m4b]  Add the chapters in [chapters.txt] to the audiobook in quicktime format."
    echo   "    -d, --dump      [audiobook.m4B]                 Dump the chapters from the audiobook to [audiobook.chapters.txt]"
}

function Add {
    chaptersFile="$1"
    audiobook="$2"

    #import chapters in nero format
    echo "Importing chapters from file: $chaptersFile"
    MP4Box -chap "$chaptersFile" "$audiobook"

    #convert to quicktime format
    mp4chaps --convert --chapter-qt "$audiobook"
}

function Dump {
    audiobook="$1"

    echo "Dumping chapters from audiobook: \"$audiobook\""
    mp4chaps --export "$audiobook"
}

case "$1" in
    -a|--add) shift; Add "$@" ;;
    -d|--dump) shift; Dump "$@" ;;
    -h|--help) PrintHelp ;;
    *) echo "Unknown action: '$1'"
esac