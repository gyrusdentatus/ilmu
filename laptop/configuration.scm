;; TODO: investigate whether to set XDG_DATA_HOME .cache or to keep it at .local/share
(use-modules (gnu)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))
(use-package-modules wm)
(use-service-modules
  cups
  desktop
  sound
  networking
  ssh
  xorg)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
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
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device "/dev/sda3")
             (type "ext4"))
           %base-file-systems)))

;/dev/sda2: UUID="19ae1dc2-a19e-4d64-89b0-c6df02ff0937" TYPE="swap" PARTUUID="28782707-02"
;/dev/sda3: LABEL="my-root" UUID="1885360f-654e-437f-bdf9-77fac7afd392" BLOCK_SIZE="4096" TYPE="ext4" PARTUUID="28782707-03"
;/dev/sda1: UUID="903A-97A6" BLOCK_SIZE="512" TYPE="vfat" PARTUUID="28782707-01"
