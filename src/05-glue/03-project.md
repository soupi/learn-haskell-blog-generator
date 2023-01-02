# Defining a project description

Up until now we've only used `base` and the libraries
[included](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/9.0.1-notes.html#included-libraries)
with GHC. Because of that we didn't really need to do anything fancier
than `runghc` to run our program. However, we want to start using
external libraries which are not included with GHC in our programs.

External packages can be downloaded from [Hackage](https://hackage.haskell.org/) -
Haskell's central package archive, [Stackage](https://www.stackage.org/) -
a subset of Hackage packages that are known to work together, or even
from remote git repositories. Usually Haskellers use a **package manager** to
download and manage packages for different projects. The most popular package
managers for Haskell are [cabal](https://cabal.readthedocs.io) and
[stack](https://haskellstack.org).

A major difference between the two is their philosophy.
`cabal` tries to be a more minimalist tool that handles building Haskell projects,
doing package management using the whole of Hackage, and uses complicated algorithms
to make sure packages work together.
`stack` tries to be a more maximalistic tool that handles installing the right GHC
for each project, provides integration with external tools like hoogle,
and lets the user choose which 'set' of packages (including their versions) they want to use.

If you've installed Haskell using GHCup, you most likely have `cabal` installed.
If you've installed Haskell using stack, well, you have `stack` installed.
Check the [haskell.org downloads page](https://www.haskell.org/downloads/) if that's not the case.

## Creating a project

Using external packages can be done in multiple ways.
For quick experimentation, we can just
[ask stack or cabal](https://gilmi.me/blog/post/2021/08/14/hs-core-tools#using-external-packages-in-ghci)
to build or even run our program with external packages.
But as programs get larger, use more dependencies, and require more functionality,
it is better to **create a project description** for our programs and libraries.

Project description is done in a **cabal file**. We can ask cabal or stack
to generate one for us using `cabal init --libandexe` or `stack new`,
along with many other files, but we will likely need to edit the file by hand
later. For now let's just paste an initial example in `hs-blog.cabal` and edit it.

```cabal
cabal-version:       2.4

name:                name should match with <name>.cabal
version:             version should use PvP
synopsis:            Synopsis will appear in the hackage package listing and search
description:         The description will appear at the top of a library
homepage:            Homepage url
bug-reports:         issue-tracker url
license:             License name
license-file:        License file
author:              Author name
maintainer:          Maintainer email
category:            Hackage categories, separated by commas
extra-doc-files:
  README.md

common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall

library
  import: common-settings
  hs-source-dirs: src
  build-depends:
      base
    , directory
  exposed-modules:
    HsBlog
      HsBlog.Convert
      HsBlog.Html
        HsBlog.Html.Internal
      HsBlog.Markup
  -- other-modules:

executable hs-blog-gen
  import: common-settings
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      base
    , <package-name>
  ghc-options:
    -O
```

Let's break it down to a few parts, the
[package metadata](#package-metadata),
[common settings](#common-settings),
[library](#library) and
[executable](#executable).

### Package metadata

The first part should be fairly straightforward from the comments, maybe except for:

- `cabal-version`: Defines which cabal versions can build this project. We've specified 2.4 and above.
  [More info on different versions](https://cabal.readthedocs.io/en/3.6/file-format-changelog.html).
- `name`: The name of your library and package. Must match with the `.cabal` filename. Usually starts with a lowercase. [Check if your package name is already taken on Hackage](https://hackage.haskell.org/packages/search?terms=name).
- `version`: Some Haskell packages use [semver](https://semver.org/), most use [PvP](https://pvp.haskell.org/).
- `license`: Most Haskell packages use [BSD-3-Clause](https://choosealicense.com/licenses/bsd-3-clause/). [Neil Mitchell blogged about this](https://neilmitchell.blogspot.com/2018/08/licensing-my-haskell-packages.html). You can find more licenses if you'd like at [choosealicense.com](https://choosealicense.com).
- `extra-doc-files`: Include extra doc files here, such as `README` or `CHANGELOG`.

Let's fill this with the metadata of our project:

```cabal
cabal-version:       2.4

name:                hs-blog
version:             0.1.0.0
synopsis:            A custom blog generator from markup files
description:         This package provides a static blog generator
                     from a custom markup format to HTML.
                     It defines a parser for this custom markup format
                     as well as an html pretty printer EDSL.

                     It is used as the example project in the online book
                     'Learn Haskell Blog Generator'. See the README for
                     more details.
homepage:            https://github.com/soupi/learn-haskell-blog-generator
bug-reports:         https://github.com/soupi/learn-haskell-blog-generator/issues
license:             BSD-3-Clause
license-file:        LICENSE.txt
author:              Gil Mizrahi
maintainer:          gilmi@posteo.net
category:            Learning, Web
extra-doc-files:
  README.md
```

### Common settings

Cabal package descriptions can include multiple "targets": libraries, executables,
and test suites. Since Cabal 2.2, we can use
[common stanzas](https://cabal.readthedocs.io/en/3.6/cabal-package.html#common-stanzas)
to group settings to be shared between different targets, so we don't have to repeat them for each target.

In our case we've created a new common stanza (or block) called `common-settings` and
defined the default language (Haskell has two standards, 98 and 2010),
and instructed GHC to compile with `-Wall`.

```cabal
common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall
```

Later, in our targets' descriptions, we can add `import: common-settings` ,
and all of these settings will be automatically added.

### Library

In a `library` target, we define:

- The settings with which to build the library (in this case we just import `common-settings`)
- The directory in which the source files can be found
- The packages we require to build the library
- The modules exposed from the library and can be used by others
- The modules *not* exposed from the library and which *cannot* be used by others;
  these could be any module you don't wish to export, such as an internal utility
  functions module.
  In our case we don't have anything like this, so we commented out the `other-modules`
  label.

Note that it is common to specify **version bounds** for packages.
Version bounds specify *which package versions this library works with*.
These can also be generated using cabal with the `cabal gen-bounds` command.

```cabal
library
  import: common-settings
  hs-source-dirs: src
  build-depends:
      base
    , directory
  exposed-modules:
    HsBlog
      HsBlog.Convert
      HsBlog.Html
        HsBlog.Html.Internal
      HsBlog.Markup
  -- other-modules:
```

Also note that we've added an additional *hierarchy* for our modules and defined
a different source directory. This means we will need to move the files around
a bit and change the `module` name in each file and the `import` statements. This is to avoid
conflict with other packages that a user might import.

---

Do this now.

<details><summary>Solution</summary>

1. `Main.hs` -> `src/HsBlog.hs`

   ```hs
   module HsBlog
     ( main
	 , process
	 )
     where

   import qualified HsBlog.Markup as Markup
   import qualified HsBlog.Html as Html
   import HsBlog.Convert (convert)
   ```

2. `Convert.hs` -> `src/HsBlog/Convert.hs`

   ```hs
   module HsBlog.Convert where

   import qualified HsBlog.Markup as Markup
   import qualified HsBlog.Html as Html
   ```

3. `Html.hs` -> `src/HsBlog/Html.hs`

   ```hs
   module HsBlog.Html
   ...

   import HsBlog.Html.Internal
   ```

4. `Html/Internal.hs` -> `src/HsBlog/Html/Internal.hs`

   ```hs
   module HsBlog.Html.Internal where
   ```


5. `Markup.hs` -> `src/HsBlog/Markup.hs`

   ```hs
   module HsBlog.Markup
   ```

</details>

---

### Executable

We have separated our code into two sections: a library and an executable, why?

First, libraries can be used by others. If we publish our code and someone wants to
use it and build upon it, they can. Executables can't be imported by other projects.
Second, we can write unit tests for libraries. It is usually
beneficial to write most, if not all, of our logic as a library, and provide
a thin executable over it.

Executables' descriptions are very similar to libraries, here we define:

- The name of the executable
- Where the source directory for this application is
- Which file is the 'Main' file
- Import our library, which is named `hs-blog`
- Additional flags for GHC, e.g., `-O` to compile with optimizations

```cabal
executable hs-blog-gen
  import: common-settings
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      base
    , hs-blog
  ghc-options:
    -O
```

We can write many executables descriptions. In this case we only have one.

---

**Exercise**: Add a new file: `app/Main.hs` which imports `HsBlog` and runs `main`.

<details><summary>Solution</summary>

```hs
-- app/Main.hs

module Main where

import qualified HsBlog

main :: IO ()
main = HsBlog.main
```

</details>

---

### Test-suites

`test-suite` defines a target for running package tests. We will get back to it
in a later chapter.

## Our complete .cabal file

```cabal
cabal-version:       2.4

name:                hs-blog
version:             0.1.0.0
synopsis:            A custom blog generator from markup files
description:         This package provides a static blog generator
                     from a custom markup format to HTML.
                     It defines a parser for this custom markup format
                     as well as an html pretty printer EDSL.

                     It is used as the example project in the online book
                     'Learn Haskell Blog Generator'. See the README for
                     more details.
homepage:            https://github.com/soupi/learn-haskell-blog-generator
bug-reports:         https://github.com/soupi/learn-haskell-blog-generator/issues
license:             BSD-3-Clause
license-file:        LICENSE.txt
author:              Gil Mizrahi
maintainer:          gilmi@posteo.net
category:            Learning, Web
extra-doc-files:
  README.md

common common-settings
  default-language: Haskell2010
  ghc-options:
    -Wall

library
  import: common-settings
  hs-source-dirs: src
  build-depends:
      base
    , directory
  exposed-modules:
    HsBlog
      HsBlog.Convert
      HsBlog.Html
        HsBlog.Html.Internal
      HsBlog.Markup
  -- other-modules:

executable hs-blog-gen
  import: common-settings
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      base
    , hs-blog
  ghc-options:
    -O
```

We'll also add a `README.md` file and a `LICENSE.txt` file:

<details><summary>README.md</summary>

Just write whatever you want here:

```md
# hs-blog

One day it will be a static blog generator.

[Read the book](https://lhbg-book.link).
```

</details>

<details><summary>LICENSE.txt</summary>

This is BSD-3-Clause with me as the author. Please write your own name for your projects :)

```
BSD 3-Clause License

Copyright (c) 2021-2022, Gil Mizrahi
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

</details>

## `cabal.project` and `stack.yaml`

The [cabal.project](https://cabal.readthedocs.io/en/3.6/cabal-project.html) and
[stack.yaml](https://docs.haskellstack.org/en/stable/yaml_configuration/#project-specific-config)
files are used by `cabal` and `stack` respectively to add additional information on *how
to build the package*. While `cabal.project` isn't necessary to use `cabal`, `stack.yaml`
is necessary in order to use `stack`, so we will cover it briefly.

There are two important fields a `stack.yaml` file must have:

- `resolver`: Describes which snapshot to use for packages and ghc version.
  We will choose the latest (at time of writing) on the `lts` branch: `lts-18.22`.
  Visit [this link](https://www.stackage.org/lts-18.22) to find out which packages this
  snapshot includes, what their versions are, and which GHC version is used
  with this snapshot
- `packages`: Describes the location of packages we plan to build. In our case
  we have only one and it can be found in the current directory

We'll add `stack.yaml` to our project directory:

```yaml
resolver: lts-18.22

packages:
- .
```

For additional options and configurations, please consult the relevant user guides.

## Usage

Now, instead of manually running `runghc Main.hs`, we will use either `stack`
or `cabal` to build and run our program and package (I mostly use stack, but it's up to you).

### For cabal:

Building the project - on the first run, cabal will download the package dependencies
and use the GHC on PATH to build the project.

Cabal caches packages between projects, so if a new project uses the same packages
with the same versions (and the same flag settings) they will not need to be reinstalled.

> In older versions of cabal, packages could be installed either globally, or in sandboxes.
> In each sandbox (and globally) there could only be one version of a package installed,
> and users would usually create different sandboxes for different projects, without caching
> packages between projects.
>
> With the new build system implementation, multiple versions of the same package can be
> installed globally, and for each project cabal will (try to) choose a specific version for each
> package dependency such that they all work together, without needing sandboxing.
> This change helps us increase sharing of built packages while avoiding conflicts and manual
> handling of sandboxes.

> Note: The new build system implementation is now the default and Cabal commands do not need
> to be prefixed with `v2-`, but the Cabal documentation will still mention the prefix to
> refer to the new commands.

A few important commands we should be familiar with:

```sh
cabal update
```

[`update`](https://cabal.readthedocs.io/en/3.6/cabal-commands.html#cabal-v2-update)
fetches information from remote package repositories (specifically Hackage unless specified otherwise)
and updates the local package index which includes various information about available packages such as
their names, versions and dependencies.

`cabal update` is usually the first command to run before fetching package dependencies.

```sh
cabal build
```

[`build`](https://cabal.readthedocs.io/en/3.6/cabal-commands.html#cabal-v2-build)
compiles the various targets (such as `library` and `executable`s).
It will also fetch and install the package dependencies when they're not already installed.

```sh
cabal run hs-blog-gen -- <program arguments>
```

[`run`](https://cabal.readthedocs.io/en/3.6/cabal-commands.html#cabal-v2-run)
Can be used to compile and then run a target (in our case our `executable` which we named `hs-blog-gen`).
We separate arguments passed to `cabal` and arguments passed to our target program with `--`.

```sh
cabal repl hs-blog
```

[`repl`](https://cabal.readthedocs.io/en/3.6/cabal-commands.html#cabal-v2-repl)
runs `ghci` in the context of the target (in our case our `library` which we named `hs-blog`) -
it will load the target's package dependencies and modules to be available in `ghci`.

```sh
cabal clean
```

[`clean`](https://cabal.readthedocs.io/en/3.6/cabal-commands.html#cabal-v2-clean)
Deletes the build artifacts that we built.

There are more interesting commands we could use, such as `cabal freeze` to generate
a file which records the packages versions and flags we used to build this project,
and `cabal sdist` to bundle the project source to a package tarball which can be
uploaded to Hackage. If you'd like to learn more visit the
[Cabal user guide](https://cabal.readthedocs.io/en/3.6/cabal-commands.html).

### For stack:

Building the project - on the first run, stack will install the right GHC for this project
which is specified by the `resolver` field in the `stack.yaml` file,
download the package dependencies, and compile the project.

Stack caches these installations between projects that use the same resolver,
so future projects with the same resolver and future runs of this project won't
require reinstallation. This approach is kind of a middle ground between full packages
sharing and sandboxes.

Let's look at the (somewhat) equivalent commands for Stack:

```sh
stack build
```

[`build`](https://docs.haskellstack.org/en/stable/build_command/#build-command)
will compile the project as described above - installing GHC and package dependencies if they are not
installed.

```sh
stack exec hs-blog-gen -- <program arguments>
```

[`exec`](https://docs.haskellstack.org/en/stable/GUIDE/#stack-exec)
will run the executable passing the program arguments to our executable.

```sh
stack ghci hs-blog
```

[`ghci`](https://docs.haskellstack.org/en/stable/ghci/#ghci)
runs `ghci` in the context of our library `hs-blog` - loading the library modules
and packages.

```sh
stack clean
```
[`clean`](https://docs.haskellstack.org/en/stable/GUIDE/#cleaning-your-project)
cleans up build artifacts.

The [Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE/) contains more
information about how stack works and how to use it effectively.

### Build artifacts

Both stack and cabal create build artifacts that we will not want to track using
our version control. These build artifacts are found in the `dist`, `dist-newstyle`
and `.stack-work` directories. We can add these to a `.gitignore` file
(or similar for other version control programs) to ignore them:

```txt
dist
dist-newstyle
.stack-work
```

## Finding packages

Finding packages isn't a very straightforward process at the moment.
People have written on
[how they choose packages](https://www.haskellforall.com/2018/05/how-i-evaluate-haskell-packages.html),
[recommendation](https://github.com/soupi/haskell-study-plan#useful-packages) [lists](https://haskelliseasy.readthedocs.io/en/latest/), and more.

My suggestion is:

- Search for a tutorial on something you'd like to do, and see which packages come up
- Use the download amount on Hackage as an indication of package popularity
- Use [Stackage](https://www.stackage.org/lts) package synopses to locate a relevant package
- Check social network channels for recommendations, but know that sometimes people tend
  to recommend inappropriate solutions and packages that might be too complicated or
  still experimental

It's also important to note the amount of dependencies a package has. Adding many dependencies
will affect compilation time and code size. And it can sometimes be a good thing to consider
when comparing packages, or considering whether a package is needed at all.

## Summary

We've created a package description for our library and used `stack` and/or `cabal`
to build our program. In future chapters we'll start adding external packages,
we'll only have to add them to the `build-depends` section in the cabal file and
our package manager will download and install the required package for us!

We've made some change to our project directory, and it should now look like this:

```
.
├── app
│   └── Main.hs
├── hs-blog.cabal
├── LICENSE.txt
├── README.md
├── src
│   ├── HsBlog
│   │   ├── Convert.hs
│   │   ├── Html
│   │   │   └── Internal.hs
│   │   ├── Html.hs
│   │   └── Markup.hs
│   └── HsBlog.hs
└── stack.yaml

4 directories, 10 files
```

Note that this package format could be released on [Hackage](https://hackage.haskell.org/)
for other Haskell developers to use!

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/8ca58aef80930db82cd20e85f44f5e34e1d74214)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/8ca58aef80930db82cd20e85f44f5e34e1d74214).
