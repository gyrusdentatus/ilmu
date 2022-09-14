;; TODO: investigate whether to set XDG_DATA_HOME .cache or to keep it at .local/share
(use-modules (gnu))
(use-package-modules wm)
(use-service-modules
  cups
  desktop
  sound
  networking
  ssh
  xorg)

(operating-system
  (locale "en_IE.utf8")
  (timezone "Europe/London")
  (keyboard-layout
    (keyboard-layout "us,is" "altgr-intl" 
		     #:options '("grp:win_space_toggle" "caps:super" "grp_led:caps")))
  (host-name "vegur")
  (users (cons* 
                (user-account
                  (name "ilmu")
                  (comment "ilmu")
                  (group "users")
                  (home-directory "/home/ilmu")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (map specification->package ;; TODO: clean this up
        (list "git" "xclip" "kitty" "neovim" "pavucontrol" "curl"
            "emacs-geiser" "emacs-evil" "emacs-evil-collection" 
            "emacs-sly" "sbcl" "stumpwm-with-slynk" "sbcl-slynk"
            "emacs" "guile" "zathura" "zathura-pdf-mupdf" "tree"
	    "ungoogled-chromium" "setxkbmap" "font-dejavu" "fd"
            "sbcl-stumpwm-ttf-fonts" "qutebrowser" "xterm" 
	    "nyxt" "nss-certs"))

      (list `(,stumpwm "lib"))
      %base-packages))
  (services
    (append
      (list (service openssh-service-type)
            (service cups-service-type)
            ;; (service alsa-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      (keyboard-layout keyboard-layout)))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid "0d7eb999-93d9-4ffc-b4b9-fe17099dcae6"))
            (target "cryptroot")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
           %base-file-systems)))
