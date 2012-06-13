# AppArmor profile for chickadee
# Place in /etc/apparmor.d and reload, or run:
# apparmor_parser --replace [--complain] /path/to/this/profile

#include <tunables/global>
@{CHICKEN}=/usr/local/chicken-4.7.0-st
@{LOGDIR}=/var/log/chickadee

# This profile assumes the use of a "trampoline" script which
# just execs the chickadee binary with the correct options and config.
# You may skip the script and assign the profile directly to the
# chickadee binary, if no one else is invoking the binary.

# WARNING: contrary to docs(?), variables don't work in profile names

profile /usr/local/bin/chickadee-prod.sh {
    #include <abstractions/base>

    # interpreter needs read access to this script
    /usr/local/bin/chickadee-prod.sh r,

    # chickadee binary inherits our perms
    @{CHICKEN}/bin/chickadee ix,

    # read/mmap access to chicken repository
    @{CHICKEN}/lib/libchicken.so.6 rm,
    @{CHICKEN}/lib/**.so rm,
    @{CHICKEN}/lib/** r,

    # read access to chickadee & chicken-doc data files
    @{CHICKEN}/share/chicken/chickadee/** r,
    @{CHICKEN}/share/chicken/chicken-doc/** r,

    # write access to chickadee logs; we really only need create & append
    @{LOGDIR}/access.log w,
    @{LOGDIR}/error.log w,
    @{LOGDIR}/ajax.log w,
    @{LOGDIR}/debug.log w,

    # AppArmor < 3.0 doesn't support finer network access control 
    network tcp,

    # silently deny harmless tty access on startup.  allow /dev/tty is probably ok too
    deny /dev/tty rw,
    deny /dev/pts/* rw,
}
