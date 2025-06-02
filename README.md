# bitestring.github.io

### Licenses

1.  Source Code

    Source code of the blog is available under [GNU Affero General Public License Version 3](https://www.gnu.org/licenses/agpl-3.0.txt).

2.  Blog Content

    All the blog content are available under
    [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA
    4.0)
    License](http://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1).

### Prerequisites for development

-   Nix (or NixOS) - <https://nixos.org>
-   Emacs for editing Org mode documents

### Developing

Enter into development shell using on root directory of this project

``` example
nix shell
```

### Running & watching

On Nix shell,

``` example
stack run site watch
```

### Building

Once inside Nix shell, use Stack to build the project

``` example
stack run site build
```

### Cleaning artifacts

``` example
stack run site clean
```
