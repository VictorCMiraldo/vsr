#! /bin/bash
wd="${BASH_SOURCE%/*}"
vsr_root="${wd}/../.."
source "$vsr_root/prelude"

set -euo pipefail

# Install firefox from flathub through flatpak. This makes life much easier
# with using the latest version of firefox on Debian and I have no weird file-dialog
# crashes. After this install, you'll need to logout for firefox to show up as a
# recognized application.
flatpak install https://dl.flathub.org/repo/appstream/org.mozilla.firefox.flatpakref

# Additionally, we need to fix the fontconfig according to:
# https://bugzilla.mozilla.org/show_bug.cgi?id=1621915
mkdir -p ~/.var/app/org.mozilla.firefox/config/fontconfig/fonts.conf
echo <<EOF > ~/.var/app/org.mozilla.firefox/config/fontconfig/fonts.conf
<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
    <!-- Disable bitmap fonts. -->
    <selectfont><rejectfont><pattern>
        <patelt name="scalable"><bool>false</bool></patelt>
    </pattern></rejectfont></selectfont>
</fontconfig>
EOF
