# Synchronizing to Android devices

It is a massive pain due to uncountable layers of virtualizations.
This directory contains a copy of [adb-sync](https://github.com/google/adb-sync) which
is, at the moment of writing, the best option for sync'ing files to an android device.
It does require `android-tools-adb` to work and an android device in "Debug Mode";
to activate debug mode click a number of times on the build number on settings, then enable USB debugging under developer options.

# Older Attempts

This directory also contains some older attempts of mine to do some glorious manual 
rsync'ing; I will keep those explorations around because I'm fully expecting 
Google to keep on making "amazing" changes on android, making it harder and harder to use.
As a consequence, it should be expected that `adb-sync` will stop working without warning.

## Actual `rsync`

For some reason, I can't manage to find my tabled mounted under `run/user/1000/gvfs` in
Debian Bullseye; hence the previous `vsr-bib-sync` script that used rsync stopped
working. That one is easily available through the git history if we ever find the problem.
Before I discovered `adb-sync`, I tried some other rudimentary methods described below:

### List the contents of a folder with `gio`

```bash
# Recursively list all files that are located under a given folder, passed as an argument.
# Additionally, list the size and modification time of those files separated by spaces.
function gio_list_recursive() {
  local queue=($1)
  local result=()

  while [[ "${#queue[*]}" -gt "0" ]]; do
      # Pops the queue head
      hd=${queue[0]}
      queue=( ${queue[@]:1} )

      # Now we move onto listing the head location and enqueuing any potential directories
      while IFS= read -r line; do
        name=$(echo "$line" | cut -f1 | sed 's/ /%20/g')
        typ=$(echo "$line" | cut -f3)

        # If we're seeing another directory, enqueue it, otherwise, add to a list of resuts.
        if [[ "$typ" == "(directory)" ]]; then
          queue+=( "$hd/$name" )
        elif [[ "$typ" == "(regular)" ]]; then
          size=$(echo "$line" | cut -f2)
          modtime=$(echo "$line" | cut -f4 | cut -d' ' -f1 | cut -d'=' -f2)
          result+=( "$hd/$name $size $modtime" )
        fi 
      done < <(gio list -a standard::type,time "$hd")
  done

  # Print the results one by one
  for i in $(seq 1 "${#result[*]}"); do
    i=$(( $i - 1 ))
    echo "${result[$i]}"
  done
}
```

This can be used with:
```bash
## Path to the virtual folder we want to sync
host="SAMSUNG_SAMSUNG_Android_R52R30PHATN/Internal%20storage/Documents"
gio_dest="mtp://$host"

device_files=$(mktemp /tmp/dev-files.XXXX)
echo "Getting list of files and mod-times in $gio_dest"
echo "  Saving it to $device_files"

if ! $(gio info "$gio_dest" > /dev/null); then
  echo "Tablet not mounted"
fi

gio_list_recursive "$gio_dest" | sed "s!$gio_dest/!!" > $device_files
```

### Manual rsync'ing

Which in turn, enables us to manually compare the diff times of files on the tablet
with files locally:
```bash
removed=()
mods=()
while IFS= read -r line; do
  # Don't forget to de-sanitize the spaces on paths
  name=$(echo "$line" | cut -d' ' -f1 | sed 's/%20/ /g')
  devsize=$(echo "$line" | cut -d' ' -f2)
  devmodtime=$(echo "$line" | cut -d' ' -f3)

  # Get the same stats of the local replica of the file in question then
  # decide what to; we might have 'removed' that file locally or modified it.
  statline=$(stat --format '%s %X' "$src/$name" 2>/dev/null)
  if [[ "$?" -eq 1 ]]; then
    echo "File $name doesn't exist in $src"
    removed+=( "$name" )
  else
    size=$(echo "$statline" | cut -d' ' -f1)
    modtime=$(echo "$statline" | cut -d' ' -f2)

    if [[ "$devmodtime" -gt "$modtime" ]]; then
      echo "File $name was modified on the tablet"
      mods+=( "$name" )
    fi
    # Probably we should also check whether the file was modified on the computer too...
  fi
done < $device_files
```


