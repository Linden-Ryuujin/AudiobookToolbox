
function PrintHelp {
    echo "chapters.sh ACTION [ACTION PARAMTERS]"
    echo
    echo Actions:
    echo   "    -a --add [chapters.txt] [audiobook.mb4]         Add the chapters in [chapters.txt] to the audiobook in quicktime format."
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

case "$1" in
    -a|--add) shift; Add "$@" ;;
    -h|--help) PrintHelp ;;
    *) echo "Unknown action: '$1'"
esac