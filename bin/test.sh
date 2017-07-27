#!/bin/sh

usage() {
    echo "usage is: ..."
}

# Initialize all the option variables.
# This ensures we are not contaminated by variables from the environment.

while :; do
    case $1 in
        -h|-\?|--help)
            # ./test.sh -?  or ./test.sh -h or ./test.sh -help
            usage    # Display a usage synopsis.
            exit
            ;;
        -f|--ipv4)
            if [ "$2" ]; then
                file=$2
                shift
            else
                die 'ERROR: "--file" requires a non-empty option argument.'
            fi
            ;;
        -?*)
            printf 'WARN: Unknown option (ignored): %s\n' "$1"
            ;;
        *)               # Default case: No more options, so break out of the loop.
            break
    esac

    shift
done

# if --file was provided, open it for writing, else duplicate stdout
if [ "$file" ]; then
    exec 3> "$file"
else
    exec 3>&1
fi

# Rest of the program here.
# If there are input files (for example) that follow the options, they
# will remain in the "$@" positional parameters.

