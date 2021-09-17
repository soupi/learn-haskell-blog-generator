# Defining a project description

Up until now we've only used `base` and the libraries
[included](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/9.0.1-notes.html#included-libraries)
with GHC. Because of that we didn't really need to do anything more fancy
than `runghc` to run our program. However, we want to start using
external libraries which are not included with GHC in our programs.

External packages can be downloaded from [Hackage](https://hackage.haskell.org/),
Haskell's central package archive, [Stackage](https://www.stackage.org/),
a subset of Hackage packages that are known to work together, or even
from remote git repositories. Usually Haskellers uses a **package manager** to
download and manage packages for different projects. The most popular package
managers for Haskell are [cabal](https://cabal.readthedocs.io) and
[stack](https://haskellstack.org).

The main difference between the two tools is their philosophy.
`cabal` tries to be a more minimalist tool that handles building Haskell projects
and package management using the whole of Hackage and uses complicated algorithms
to make sure packages work together.
`stack` tries to be a more maximal tool that handles installing the right GHC
for each project, provide integration with external tools like hoogle,
and lets the user choose which 'set' of packages (including their versions) they want to use.

If you've installed Haskell using GHCup, you most likely have `cabal` installed.
If you've installed Haskell using stack, well, you have `stack` installed.
Check the [haskell.org downloads page](https://www.haskell.org/downloads/) if that's not the case.

## Creating a project

Using external packages can be done in multiple ways.
For quick experimentation, we can just
[ask stack or cabal](https://gilmi.me/blog/post/2021/08/14/hs-core-tools#using-external-packages-in-ghci)
to build or even run our program with external packages.
But as programs gets larger, use more dependencies, and require more functionality,
it is better to just **create a project description** for our programs and even libraries.

Describing packages is done in a **cabal file**. We can ask cabal or stack
to generate one for use using `cabal init --libandexe` or `stack new`,
along with many other files, but we will likely need to edit the file by hand
later so let's just paste some sort of an initial example and then edit it.

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

Let's break it down to a few parts, the **package metadata**, **common settings**,
**library** and **executable**.

### Package metadata

The first part should be fairly straightforward from the comments, maybe except for:

- `cabal-version`: Defines which cabal versions can build this project. We've specified 2.4 and above.
  [More info on different versions](https://cabal.readthedocs.io/en/3.4/file-format-changelog.html?highlight=cabal-version).
- `name`: The name of your library and package. Must match with the .cabal filename. Usually starts with a lowercase. [Check if your package name is already taken on Hackage](https://hackage.haskell.org/packages/search?terms=name).
- `version`: Some Haskell packages use [semver](https://semver.org/), most use [PvP](https://pvp.haskell.org/).
- `license`: Most Haskell packages use [BSD-3-Clause](https://choosealicense.com/licenses/bsd-3-clause/). [Neil Mitchell blogged about this](https://neilmitchell.blogspot.com/2018/08/licensing-my-haskell-packages.html). The license we chose is because this is a book learning material.
- `extra-doc-files`: Include extra doc files here, such as `README` or `CHANGELOG`.

Let's fill this with the metadata of our project:

```cabal
cabal-version:       2.4

name:                hs-blog
version:             0.1.0.0
synopsis:            A custom blog generator from markup files
description:         This package provides a static blog generator
                     from a custom markup format to HTML.
                     It defines a parsing for this custom markup format
                     as well as an html pretty printer EDSL.

                     It is used as the example project in the online book
                     'Learn Haskell Blog Generator'. See the README for
                     more details.
homepage:            https://github.com/soupi/learn-haskell-blog-generator
bug-reports:         https://github.com/soupi/learn-haskell-blog-generator/issues
license:             CC0-1.0
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
[common stanzas](https://cabal.readthedocs.io/en/3.4/cabal-package.html?highlight=common#common-stanzas)
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

Later, in our targets descriptions, we can add `import: common-settings` ,
and all of these settings will be automatically added.

<!--

> #### Aside: language extensions
>
> Haskell is a standardized language. However, GHC provides *extensions* to the language -
> additional features that aren't covered in the 98 or 2010 standards of Haskell.
> Features such as syntactic extensions, extensions to the type checker, and more.
> These extensions can be added by adding `{-# language <extension-name> #-}`
> to the top of a Haskell source file, or they can be set by *default* in the `.cabal file`.
> 
> As an example, we've enabled
> [NumericUnderscores](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/numeric_underscores.html),
> which allows us to add arbitrary underscores (`_`) in numeric literals for ease of reading.
> For example, instead of writing `1000000` we can write `1_000_000`. We don't actually
> need this at the moment, but it's nice to know about it.
> 
> The list of language extensions can be found in the
> [GHC manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts.html), but don't worry
> about it just yet, we will cover more extensions later.

-->

### Library

In a `library` target, we define:

- The settings with which to build the library (in this case we just import `common-settings`)
- The directory in which the source files can be found
- The packages we require to build the library
- The modules exposed from the library and can be used by others
- The modules *not* exposed from the library and which *cannot* be used by others,
  these could be any module you don't wish to export, such as an internal utility
  functions module.
  In our case we don't have anything like this, so we commented out the `other-modules`
  label.

Note that it is common to specify **version bounds** for packages.
Version bounds specify *with which packages version this library works with*.
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

Do this now!

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

### Executable

We have separated our code to two sections: a library and an executable, why?

First, libraries can be used by others. if we publish our code and someone wants to
use it and build upon it, they can. Executables can't be imported to other projects.
Second, we can write unit tests for libraries, which we will do soon. It is usually
benefitical to write most, if not all, of our logic as a library, and provide
a thin executable over it.

Executables' descriptions are very similar to libraries, here we define:

- The name of the executable
- Where the source directory for this application is
- Which file is the 'Main' file
- Import our library, which is named `hs-blog`
- Add additional flag for GHC: `-O` to compile with optimizations

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

We can write many executables descriptions. In this case we only need one.

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
                     It defines a parsing for this custom markup format
                     as well as an html pretty printer EDSL.

                     It is used as the example project in the online book
                     'Learn Haskell Blog Generator'. See the README for
                     more details.
homepage:            https://github.com/soupi/learn-haskell-blog-generator
bug-reports:         https://github.com/soupi/learn-haskell-blog-generator/issues
license:             CC0-1.0
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

[Read the book](https://soupi.github.io/learn-haskell-blog-generator).
```

</details>

<details><summary>LICENSE.txt</summary>

This is CC0-1.0 Universal

```
Creative Commons Legal Code

CC0 1.0 Universal

    CREATIVE COMMONS CORPORATION IS NOT A LAW FIRM AND DOES NOT PROVIDE
    LEGAL SERVICES. DISTRIBUTION OF THIS DOCUMENT DOES NOT CREATE AN
    ATTORNEY-CLIENT RELATIONSHIP. CREATIVE COMMONS PROVIDES THIS
    INFORMATION ON AN "AS-IS" BASIS. CREATIVE COMMONS MAKES NO WARRANTIES
    REGARDING THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS
    PROVIDED HEREUNDER, AND DISCLAIMS LIABILITY FOR DAMAGES RESULTING FROM
    THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS PROVIDED
    HEREUNDER.

Statement of Purpose

The laws of most jurisdictions throughout the world automatically confer
exclusive Copyright and Related Rights (defined below) upon the creator
and subsequent owner(s) (each and all, an "owner") of an original work of
authorship and/or a database (each, a "Work").

Certain owners wish to permanently relinquish those rights to a Work for
the purpose of contributing to a commons of creative, cultural and
scientific works ("Commons") that the public can reliably and without fear
of later claims of infringement build upon, modify, incorporate in other
works, reuse and redistribute as freely as possible in any form whatsoever
and for any purposes, including without limitation commercial purposes.
These owners may contribute to the Commons to promote the ideal of a free
culture and the further production of creative, cultural and scientific
works, or to gain reputation or greater distribution for their Work in
part through the use and efforts of others.

For these and/or other purposes and motivations, and without any
expectation of additional consideration or compensation, the person
associating CC0 with a Work (the "Affirmer"), to the extent that he or she
is an owner of Copyright and Related Rights in the Work, voluntarily
elects to apply CC0 to the Work and publicly distribute the Work under its
terms, with knowledge of his or her Copyright and Related Rights in the
Work and the meaning and intended legal effect of CC0 on those rights.

1. Copyright and Related Rights. A Work made available under CC0 may be
protected by copyright and related or neighboring rights ("Copyright and
Related Rights"). Copyright and Related Rights include, but are not
limited to, the following:

  i. the right to reproduce, adapt, distribute, perform, display,
     communicate, and translate a Work;
 ii. moral rights retained by the original author(s) and/or performer(s);
iii. publicity and privacy rights pertaining to a person's image or
     likeness depicted in a Work;
 iv. rights protecting against unfair competition in regards to a Work,
     subject to the limitations in paragraph 4(a), below;
  v. rights protecting the extraction, dissemination, use and reuse of data
     in a Work;
 vi. database rights (such as those arising under Directive 96/9/EC of the
     European Parliament and of the Council of 11 March 1996 on the legal
     protection of databases, and under any national implementation
     thereof, including any amended or successor version of such
     directive); and
vii. other similar, equivalent or corresponding rights throughout the
     world based on applicable law or treaty, and any national
     implementations thereof.

2. Waiver. To the greatest extent permitted by, but not in contravention
of, applicable law, Affirmer hereby overtly, fully, permanently,
irrevocably and unconditionally waives, abandons, and surrenders all of
Affirmer's Copyright and Related Rights and associated claims and causes
of action, whether now known or unknown (including existing as well as
future claims and causes of action), in the Work (i) in all territories
worldwide, (ii) for the maximum duration provided by applicable law or
treaty (including future time extensions), (iii) in any current or future
medium and for any number of copies, and (iv) for any purpose whatsoever,
including without limitation commercial, advertising or promotional
purposes (the "Waiver"). Affirmer makes the Waiver for the benefit of each
member of the public at large and to the detriment of Affirmer's heirs and
successors, fully intending that such Waiver shall not be subject to
revocation, rescission, cancellation, termination, or any other legal or
equitable action to disrupt the quiet enjoyment of the Work by the public
as contemplated by Affirmer's express Statement of Purpose.

3. Public License Fallback. Should any part of the Waiver for any reason
be judged legally invalid or ineffective under applicable law, then the
Waiver shall be preserved to the maximum extent permitted taking into
account Affirmer's express Statement of Purpose. In addition, to the
extent the Waiver is so judged Affirmer hereby grants to each affected
person a royalty-free, non transferable, non sublicensable, non exclusive,
irrevocable and unconditional license to exercise Affirmer's Copyright and
Related Rights in the Work (i) in all territories worldwide, (ii) for the
maximum duration provided by applicable law or treaty (including future
time extensions), (iii) in any current or future medium and for any number
of copies, and (iv) for any purpose whatsoever, including without
limitation commercial, advertising or promotional purposes (the
"License"). The License shall be deemed effective as of the date CC0 was
applied by Affirmer to the Work. Should any part of the License for any
reason be judged legally invalid or ineffective under applicable law, such
partial invalidity or ineffectiveness shall not invalidate the remainder
of the License, and in such case Affirmer hereby affirms that he or she
will not (i) exercise any of his or her remaining Copyright and Related
Rights in the Work or (ii) assert any associated claims and causes of
action with respect to the Work, in either case contrary to Affirmer's
express Statement of Purpose.

4. Limitations and Disclaimers.

 a. No trademark or patent rights held by Affirmer are waived, abandoned,
    surrendered, licensed or otherwise affected by this document.
 b. Affirmer offers the Work as-is and makes no representations or
    warranties of any kind concerning the Work, express, implied,
    statutory or otherwise, including without limitation warranties of
    title, merchantability, fitness for a particular purpose, non
    infringement, or the absence of latent or other defects, accuracy, or
    the present or absence of errors, whether or not discoverable, all to
    the greatest extent permissible under applicable law.
 c. Affirmer disclaims responsibility for clearing rights of other persons
    that may apply to the Work or any use thereof, including without
    limitation any person's Copyright and Related Rights in the Work.
    Further, Affirmer disclaims responsibility for obtaining any necessary
    consents, permissions or other rights required for any use of the
    Work.
 d. Affirmer understands and acknowledges that Creative Commons is not a
    party to this document and has no duty or obligation with respect to
    this CC0 or use of the Work.
```

</details>

## `cabal.project` and `stack.yaml`

The [cabal.project](https://cabal.readthedocs.io/en/3.4/cabal-project.html) and
[stack.yaml](https://docs.haskellstack.org/en/stable/yaml_configuration/#project-specific-config)
files are used by `cabal` and `stack` respectively to add additional information on *how
to build the package*. While `cabal.project` isn't necessary to use `cabal`, `stack.yaml`
is necessary in order to use `stack`. So we will cover it briefly.

There are two important fields a `stack.yaml` file must have:

- `resolver`: Describes which snapshot to use for packages and ghc version.
  We will choose the latest (at time of writing) on the `lts` branch: `lts-18.9`.
  Visit [this link](https://www.stackage.org/lts-18.9) to find which packages this
  snapshot includes, what are their version, and for which GHC version is set
  for the snapshot.
- `packages`: Describes the location of packages we plan to build. In our case
  we have only one, in the current directory.

We'll add `stack.yaml` to our project directory:

```yaml
resolver: lts-18.9

packages:
- .
```

For additional options and configurations, please consult the relevant user guides.

## Usage

Now, instead of manually running `runghc Main.hs`, we will now use either `stack`
or `cabal` to build and run our program and package (I mostly use stack, but up to you).

### For cabal:

Building the project - on the first run, cabal will download the package dependencies
and use the GHC on PATH to build the project.

cabal caches packages between projects, so if a new project uses the same packages
with the same versions (and the same flag settings) they will not need to be reinstalled.

```sh
cabal v2-build
```

Running the project: `stack exec` will run the executable.

```sh
cabal v2-run hs-blog-gen -- <program options>
```

We can also run `ghci` with our project loaded:

```sh
cabal v2-repl
```


### For stack:

Building the project - on the first run, stack will install the right GHC for this project
which is specified according to the `resolver` field in the `stack.yaml` file,
will download the package dependencies, and build the project.

Stack caches these installations between projects that use the same resolver,
so future projects with the same resolver and future runs of this project won't
require reinstallation.

```sh
stack build
```

Running the project: `stack exec` will run the executable.

```sh
stack exec hs-blog-gen -- <program options>
```

Or run `ghci`:

```sh
stack ghci
```


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
- Use [Stackage](https://www.stackage.org/lts) package synopsises to locate a relevant package
- Check social network channels for recommendations, but know that sometimes people tend
  to recommend inappropriate solutions and packages that might be too complicated or
  still experimental

It's also important to note the amount of dependencies a package has. Adding many dependencies
will affect compilation time and code size. And it can sometimes be a good thing to consider
when comparing packages, or considering whether a package is needed at all.

## Summary

We've created a package description for our library and used `stack` or/and `cabal`
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

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/64fcc688c05faf01bff9a39b7e7e70871175b84f)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/code-after-optparse).
