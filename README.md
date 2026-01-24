# qutebrowser-start-page

### Install
available in [NUR]( https://nur.nix-community.org/repos/afreakk/ )  
or `nix-build` it manually

### Usage
```
qutebrowser-start-page > startpage.html
```

### Favicons
To load favicons on the start page, allow local files to access remote URLs:
```python
c.content.local_content_can_access_remote_urls = True
```

### Example usage with [home-manager](https://github.com/nix-community/home-manager):
```
let
  qutebrowser_startpage_path = "${config.home.homeDirectory}/.local/share/qutebrowser/startpage.html";
in
...
    qutebrowser = {
      enable = true;
      settings = {
        url = {
          start_pages = qutebrowser_startpage_path;
          default_page = qutebrowser_startpage_path;
        };
      };
      keyBindings = {
        normal = {
          ",s" = "spawn sh -c '${pkgs.myPackages.qutebrowser-start-page}/bin/qutebrowser-start-page > ${qutebrowser_startpage_path}'";
        };
      };
    };
```
then just press ,s to update startpage
