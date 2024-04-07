{...}:

{
  programs.zathura = {
    enable = true;
    extraConfig = ''
      set selection-clipboard clipboard
      set window-title-home-tilde true
      set statusbar-home-tilde true
      set adjust-open "best-fit"

     set font "JetBrainsMono Nerd Font"
     set adjust-open width
     set recolor true
     set recolor-keephue true
     map <C-Tab> toggle_statusbar
     set guioptions ""

     set recolor-lightcolor rgba(0,0,0,0.2)
     set recolor-darkcolor "#dce0e8"
     set highlight-active-color "#111111"
     set highlicht-color "#dce0e8"
    '';
  };

}
