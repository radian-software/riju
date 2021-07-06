#!/usr/bin/env bash

set -euo pipefail

: ${L} ${T}

deb="build/${T}/${L}/riju-${T}-${L}.deb"
tmp="build/${T}/${L}/ar-tmp"

rm -rf "${tmp}"

files="$(ar t "${deb}" | grep -F .tar | grep -Fv .xz || true)"

if [[ -z "${files}" ]]; then
    exit 0
fi

du -sh "${deb}"

echo >&2 "found files in ${deb} needing recompression:"
echo "${files}" | sed >&2 's/^/  /'

echo >&2 "extracting ${deb} to ${tmp}..."
mkdir -p "${tmp}"
ar xv "${deb}" --output "${tmp}" | sed >&2 's/^/  /'

echo >&2 "recompressing extracted files..."

processed_files=()
while read file; do
    if echo "${file}" | grep -F .tar | grep -Fvq .xz; then
        ext="$(echo "${file}" | sed -E 's/^.+\.tar//')"
        case "${ext}" in
            "")
                echo >&2 "  ${file} => ${file}.xz"
                xz "${tmp}/${file}"
                processed_files=("${processed_files[@]}" "${file}.xz")
                ;;
            ".gz")
                echo >&2 "  ${file} => ${file%.gz}.xz"
                gunzip "${tmp}/${file}" -c | xz - > "${tmp}/${file%.gz}"
                rm "${tmp}/${file}"
                processed_files=("${processed_files[@]}" "${file%.gz}")
                ;;
            *)
                echo >&2 "  !! unknown file extension in ${file}"
                exit 1
                ;;
        esac
    else
        processed_files=("${processed_files[@]}" "${file}")
    fi
done < <(ar t "${deb}")

# NB we need to keep track of the processed_files array because the
# order needs to be preserved, surprisingly enough:
# https://ubuntuforums.org/archive/index.php/t-1481153.html

echo >&2 "recreating archive at ${deb}.tmp..."
pushd "${tmp}" >/dev/null
ar rcv "../$(basename "${deb}").tmp" "${processed_files[@]}" | sed 's/^/  /'
popd >/dev/null

echo >&2 "cleaning up ${tmp}..."
rm -rf "${tmp}"

echo >&2 "renaming ${deb}.tmp => ${deb}..."
mv "${deb}.tmp" "${deb}"

files="$(ar t "${deb}" | grep -F .tar | grep -Fv .xz || true)"

if [[ -n "${files}" ]]; then
    echo >&2 "error: still found files in ${deb} needing recompression:"
    echo "${files}" | sed >&2 's/^/  /'
    exit 1
fi
