# bitestring.github.io

### Licenses

1.  Source Code

    The blog's source code is licensed under the [GNU Affero General Public License Version 3](https://www.gnu.org/licenses/agpl-3.0.txt). A copy of the GNU AGPL V3.0 license can be found in the file [LICENSE](./LICENSE) within this repository.

2.  Blog Content

    All blog content is licensed under the [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) License](http://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1). A copy of the CC BY-NC-SA 4.0 license can be found in the file [LICENSE.CC_BY-NC-SA_4.0](./LICENSE.CC_BY-NC-SA_4.0) within this repository.

### Prerequisites for development

- Nix (or NixOS) - <https://nixos.org>
- Emacs for editing Org mode documents

### Developing

Enter into development shell using on root directory of this project

```
nix shell
```

### Running & watching

On Nix shell,

```
stack run site watch
```

### Building

Once inside Nix shell, use Stack to build the project

```
stack run site build
```

### Cleaning artifacts

```
stack run site clean
```
