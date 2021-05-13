{ config, lib, ... }: with builtins; with lib; with j; {
    # For Yubikey SSH-GPG Authentication
    environment.shellInit = ''
        export GPG_TTY="$(tty)"
        gpg-connect-agent /bye
        export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
        echo UPDATESTARTUPTTY | gpg-connect-agent
    '';
    programs = {
        # Some programs need SUID wrappers, can be configured further or are
        # started in user sessions.
        # mtr.enable = true;
        gnupg.agent = {
            enable = true;
            enableSSHSupport = false;
            pinentryFlavor = "curses";
        };

        # For use with Yubikey SSH-GPG Authentication, set to false
        ssh.startAgent = true;
    };
    security.pam = {
        yubico = {
            enable = true;
            debug = true;
            mode = "challenge-response";
        };
        enableSSHAgentAuth = true;
    };
}
