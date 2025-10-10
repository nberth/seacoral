#!/bin/sh
# Force passing proxy-related environment variables via sudo. To be
# executed via sudo.

sed -i /etc/sudoers								\
    -e 's/^#\(Defaults.*env_keep.*_proxy.*\)$/\1/'

# Group defauls is not enough, so we do it for everybody.
echo >> /etc/sudoers								\
     'Defaults env_keep += "ftp_proxy http_proxy https_proxy no_proxy"'
