---
title: Stack 2.1.1
headingBackgroundImage: ../../img/background.png
headingDivClass: post-heading
author: Kazuki Okamoto, …
postedBy: <a href="https://twitter.com/kakkun61">Kazuki Okamoto (@kakkun61)</a>, …
date: June 30, 2019
tags: Stack
---

## v2.1.1

**バージョン1.9.3からの変更点**

主要な変更：

* Switch over to pantry for managing packages. This is a major change
  to Stack's internals, and affects user-visible behavior in a few
  places. Some highlights:
    * マルチパッケージインデックスと古い`00-index.tar`形式インデックスをサポートから除外。[#4137](https://github.com/commercialhaskell/stack/issues/4137)を参照せよ。
    * `package`セクションにおけるアーカイブとレポジトリーをサポートから除外。代替として、そのような依存には`extra-deps`を使用せよ。`packages`はローカルのファイルパスのみをサポートすることとなる。
    * Gitリポジトリーのsubmoduleのサポートを追加（再帰していてもよい）。
    * 「パントリー木」<!-- 原文：pantry tree 岡本：なんだそれ？ -->キーを指定するための新しい設定オプションの追加。それはビルドに関してよりよい再現性を提供し、（将来的には）より効果的なパッケージ内容のダウンロードのために使用される。より効果的な設定解析のためにパッケージ名やバージョンを指定することもできる。
          * __備考__ 新しい`stack freeze`コマンドは自動的にこの追加情報を生成するようになった。
    * ファイルシステム上のファイルの代わりにSQLiteデータベースにパッケージの内容と付加情報は保存される。`pantry`ライブラリーはこれらの内容を読み書きするために使用される。
    * 内部的にStackのデータ型の多くは変更が入った。それらはCabal側の定義への移動も含む。その結果として、既存のキャッシュファイルは一般的に無効となりすでにあるキャッシュの再ビルドが必要となる。申し訳ない。m(_ _)m
    * 新コマンド`stack freeze`の追加。これは依存をある特定のバージョンに固定するためプロジェクトとスナップショットの定義を出力する。
    * `ignore-revision-mismatch`設定はもはや必要でないため削除。
    * GHCブートパッケージを上書きすると、それに依存する他のGHCブートパッケージを依存として利用できなくなる。必要なときは、それらのパッケージを明示的に追加する必要がある。[#4510](https://github.com/commercialhaskell/stack/issues/4510)を参照せよ。
    * Cabal解決統合は新しい`cabal-install`をサポートするよう更新されなかったので、`stack new`と`stack init`から`--solver`を削除するだけでなく`stack solver`コマンドも削除された。
* Upgrade to Cabal 2.4
    * Note that, in this process, the behavior of file globbing has
      been modified to match that of Cabal. In particular, this means
      that for Cabal spec versions less than 2.4, `*.txt` will
      match `foo.txt`, but not `foo.2.txt`.
* Remove the `stack image` command. With the advent of Docker multistage
  builds, this functionality is no longer useful. For an example, please see
  [Building Haskell Apps with
  Docker](https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker).
* Support building GHC from source (experimental)
    * Stack now supports building and installing GHC from source. The built GHC
      is uniquely identified by a commit id and an Hadrian "flavour" (Hadrian is
      the newer GHC build system), hence `compiler` can be set to use a GHC
      built from source with `ghc-git-COMMIT-FLAVOUR`
* `stack.yaml` now supports a `configure-options`, which are passed directly to
  the `configure` step in the Cabal build process. See
  [#1438](https://github.com/commercialhaskell/stack/issues/1438)
* Remove support for building GHCJS itself. Future releases of Stack
  may remove GHCJS support entirely.
* Support for lock files for pinning exact project dependency versions

Behavior changes:

* `stack.yaml` now supports `snapshot`: a synonym for `resolver`. See
  [#4256](https://github.com/commercialhaskell/stack/issues/4256)
* `stack script` now passes `-i -idir` in to the `ghc`
  invocation. This makes it so that the script can import local
  modules, and fixes an issue where `.hs` files in the current
  directory could affect interpretation of the script. See
  [#4538](https://github.com/commercialhaskell/stack/pull/4538)
* When using `stack script`, custom snapshot files will be resolved
  relative to the directory containing the script.
* Remove the deprecated `--upgrade-cabal` flag to `stack setup`.
* Support the `drop-packages` field in `stack.yaml`
* Remove the GPG signing code during uploads. The GPG signatures have
  never been used yet, and there are no plans to implement signature
  verification.
* Remove the `--plain` option for the `exec` family of commands
* Always use the `--exact-configuration` Cabal configuration option when
  building (should mostly be a non-user-visible enhancement).
* No longer supports Cabal versions older than `1.19.2`.  This means
  projects using snapshots earlier than `lts-3.0` or
  `nightly-2015-05-05` will no longer build.
* Remove the `stack docker cleanup` command.  Docker itself now has
  [`docker image prune`](https://docs.docker.com/engine/reference/commandline/image_prune/)
  and
  [`docker container prune`](https://docs.docker.com/engine/reference/commandline/container_prune/),
  which you can use instead.
* Interleaved output is now turned on by default, see
  [#4702](https://github.com/commercialhaskell/stack/issues/4702). In
  addition, the `packagename> ` prefix is no longer included in
  interelaved mode when only building a single target.
* The `-fhide-source-paths` GHC option is now enabled by default and
  can be disabled via the `hide-source-paths` configuration option in
  `stack.yaml`. See [#3784](https://github.com/commercialhaskell/stack/issues/3784)
* Stack will reconfigure a package if you modify your `PATH` environment
  variable. See
  [#3138](https://github.com/commercialhaskell/stack/issues/3138).
* For GHC 8.4 and later, disable the "shadowed dependencies" workaround. This
  means that Stack will no longer have to force reconfigures as often. See
  [#3554](https://github.com/commercialhaskell/stack/issues/3554).
* When building a package, Stack takes a lock on the dist directory in
  use to avoid multiple runs of Stack from trampling each others'
  files. See
  [#2730](https://github.com/commercialhaskell/stack/issues/2730).
* Stack will check occassionally if there is a new version available and prompt
  the user to upgrade. This will not incur any additional network traffic, as
  it will piggy-back on the existing Hackage index updates. You can set
  `recommend-stack-upgrade: false` to bypass this. See
  [#1681](https://github.com/commercialhaskell/stack/issues/1681).
* `stack list-dependencies` has been removed in favour of `stack ls dependencies`.
* The new default for `--docker-auto-pull` is enabled. See
  [#3332](https://github.com/commercialhaskell/stack/issues/3332).

Other enhancements:

* Support MX Linux in get-stack.sh. Fixes
  [#4769](https://github.com/commercialhaskell/stack/issues/4769).
* Defer loading up of files for local packages. This allows us to get
  plan construction errors much faster, and avoid some unnecessary
  work when only building a subset of packages. This is especially
  useful for the curator use case.
* Existing global option `--color=WHEN` is now also available as a
  non-project-specific yaml configuration parameter `color:`.
* Adopt the standard proposed at http://no-color.org/, that color should not be
  added by default if the `NO_COLOR` environment variable is present.
* New command `stack ls stack-colors` lists the styles and the associated 'ANSI'
  control character sequences that stack uses to color some of its output. See
  `stack ls stack-colors --help` for more information.
* New global option `--stack-colors=STYLES`, also available as a
  non-project-specific yaml configuration parameter, allows a stack user to
  redefine the default styles that stack uses to color some of its output. See
  `stack --help` for more information.
* British English spelling of 'color' (colour) accepted as an alias for
  `--color`, `--stack-colors`, `stack ls stack-colors` at the command line and
  for `color:` and `stack-colors:` in yaml configuration files.
* New build option `--ddump-dir`. (See 
  [#4225](https://github.com/commercialhaskell/stack/issues/4225))
* Stack parses and respects the `preferred-versions` information from
  Hackage for choosing latest version of a package in some cases,
  e.g. `stack unpack packagename`.
* The components output in the `The main module to load is ambiguous` message
  now include package names so they can be more easily copy-pasted.
* Git repos are shared across multiple projects. See
  [#3551](https://github.com/commercialhaskell/stack/issues/3551)
* Use en_US.UTF-8 locale by default in pure Nix mode so programs won't
  crash because of Unicode in their output
  [#4095](https://github.com/commercialhaskell/stack/issues/4095)
* Add `--tree` to `ls dependencies` to list dependencies as tree.
  [#4101](https://github.com/commercialhaskell/stack/issues/4101)
* Add `--pedantic` to `ghci` to run with `-Wall` and `-Werror`
  [#4463](https://github.com/commercialhaskell/stack/issues/4463)
* Add `--cabal-files` flag to `stack ide targets` command.
* Add `--stdout` flag to all `stack ide` subcommands.
* Use batches when unregistering packages with `ghc-pkg`.
  (See [#2662](https://github.com/commercialhaskell/stack/issues/2662))
* `get-stack` script now works on Windows CI machines of Appveyor,
  Travis and Azure Pipelines. See
  [#4535](https://github.com/commercialhaskell/stack/issues/4535)/
* Show snapshot being used when `stack ghci` is invoked outside of a project
  directory. See
  [#3651](https://github.com/commercialhaskell/stack/issues/3651)
* The script interpreter now accepts a `--extra-dep` flag for adding
  packages not present in the snapshot. Currently, this only works
  with packages from Hackage, not Git repos or archives.
* When using the script interpreter with `--optimize` or `--compile`,
  Stack will perform an optimization of checking whether a newer
  executable exists, making reruns significantly faster. There's a
  downside to this, however: if you have a multifile script, and
  change one of the dependency modules, Stack will not automatically
  detect and recompile.
* `stack clean` will delete the entire `.stack-work/dist` directory,
  not just the relevant subdirectory for the current GHC version. See
  [#4480](https://github.com/commercialhaskell/stack/issues/4480).
* Add `stack purge` as a shortcut for `stack clean --full`. See
  [#3863](https://github.com/commercialhaskell/stack/issues/3863).
* Both `stack dot` and `stack ls dependencies` accept a
  `--global-hints` flag to bypass the need for an installed GHC. See
  [#4390](https://github.com/commercialhaskell/stack/issues/4390).
* Add the `stack config env` command for getting shell script environment
  variables. See [#620](https://github.com/commercialhaskell/stack/issues/620).
* Less verbose output from `stack setup` on Windows. See
  [#1212](https://github.com/commercialhaskell/stack/issues/1212).
* Add an optional `ignore-expiry` flag to the `hackage-security`
  section of the `~/.stack/config.yaml`. It allows to disable timestamp
  expiration verification just like `cabal --ignore-expiry` does.
  The flag is not enabled by default so that the default functionality
  is not changed.
* Include default values for most command line flags in the `--help`
  output. See
  [#893](https://github.com/commercialhaskell/stack/issues/893).
* Set the `GHC_ENVIRONMENT` environment variable to specify dependency
  packages explicitly when running test. This is done to prevent
  ambiguous module name errors in `doctest` tests.
* `get-stack` script now works on Windows CI machines of Appveyor,
  Travis and Azure Pipelines. See
  [#4535](https://github.com/commercialhaskell/stack/issues/4535)
* Warn when a Docker image does not include a `PATH` environment
  variable. See
  [#2472](https://github.com/commercialhaskell/stack/issues/2742)
* When using `system-ghc: true`, Stack will now find the appropriate GHC
  installation based on the version suffix, allowing you to more easily switch
  between various system-installed GHCs. See
  [#2433](https://github.com/commercialhaskell/stack/issues/2433).
* `stack init` will now support create a `stack.yaml` file without any local
  packages. See [#2465](https://github.com/commercialhaskell/stack/issues/2465)
* Store caches in SQLite database instead of files.
* No longer use "global" Docker image database (`docker.db`).
* User config files are respected for the script command. See
  [#3705](https://github.com/commercialhaskell/stack/issues/3705),
  [#3887](https://github.com/commercialhaskell/stack/issues/3887).
* Set the `GHC_ENVIRONMENT` environment variable to `-` to tell GHC to
  ignore any such files when GHC is new enough (>= 8.4.4), otherwise
  simply unset the variable. This allows Stack to have control of
  package databases when running commands like `stack exec ghci`, even
  in the presence of implicit environment files created by `cabal
  new-build`. See
  [#4706](https://github.com/commercialhaskell/stack/issues/4706).
* Use a database cache table to speed up discovery of installed GHCs
* You can specify multiple `--test-arguments` options. See
  [#2226](https://github.com/commercialhaskell/stack/issues/2226)
* Windows terminal width detection is now done. See
  [#3588](https://github.com/commercialhaskell/stack/issues/3588)
* On Windows, informs users if the 'programs' path contains a space character
  and further warns users if that path does not have an alternative short
  ('8 dot 3') name, referencing the `local-programs-path` configuration option.
  See [#4726](https://github.com/commercialhaskell/stack/issues/4726)
* Add `--docker-mount-mode` option to set the Docker volume mount mode
  for performance tuning on macOS.

Bug fixes:

* Ignore duplicate files for a single module when a Haskell module was
  generated from a preprocessor file. See
  [#4076](https://github.com/commercialhaskell/stack/issues/4076).
* Only track down components in current directory if there are no
  hs-source-dirs found. This eliminates a number of false-positive
  warnings, similar to
  [#4076](https://github.com/commercialhaskell/stack/issues/4076).
* Handle a change in GHC's hi-dump format around `addDependentFile`,
  which now includes a hash. See
  [yesodweb/yesod#1551](https://github.com/yesodweb/yesod/issues/1551)
* Fix `subdirs` for git repos in `extra-deps` to match whole directory names.
  Also fixes for `subdirs: .`. See
  [#4292](https://github.com/commercialhaskell/stack/issues/4292)
* Fix for git packages to update submodules to the correct state. See
  [#4314](https://github.com/commercialhaskell/stack/pull/4314)
* Add `--cabal-files` flag to `stack ide targets` command.
* Don't download ghc when using `stack clean`.
* Support loading in GHCi definitions from symlinked C files. Without this
  patch, Stack will try to find object files in the directory pointed to
  by symlinks, while GCC will produce the object files in the original
  directory. See
  [#4402](https://github.com/commercialhaskell/stack/pull/4402)
* Fix handling of GitHub and URL templates on Windows. See
  [commercialhaskell/stack#4394](https://github.com/commercialhaskell/stack/issues/4394)
* Fix `--file-watch` not responding to file modifications when running
  inside docker on Mac. See
  [#4506](https://github.com/commercialhaskell/stack/issues/4506)
* Using `--ghc-options` with `stack script --compile` now works.
* Ensure the detailed-0.9 type tests work.
  See [#4453](https://github.com/commercialhaskell/stack/issues/4453).
* Extra include and lib dirs are now order-dependent. See
  [#4527](https://github.com/commercialhaskell/stack/issues/4527).
* Apply GHC options when building a `Setup.hs` file. See
  [#4526](https://github.com/commercialhaskell/stack/issues/4526).
* Stack handles ABI changes in FreeBSD 12 by differentiating that version from
  previous.
* Help text for the `templates` subcommand now reflects behaviour in stack 1.9
  — that it downloads and shows a help file, rather than listing available
  templates.
* Fix detection of aarch64 platform (this broke when we upgraded to a newer
  Cabal version).
* Docker: fix detecting and pulling missing images with `--docker-auto-pull`, see
  [#4598](https://github.com/commercialhaskell/stack/issues/4598)
* Hackage credentials are not world-readable. See
  [#2159](https://github.com/commercialhaskell/stack/issues/2159).
* Warnings are dumped from logs even when color is enabled. See
  [#2997](https://github.com/commercialhaskell/stack/issues/2997)
* `stack init` will now work for cabal files with sublibraries. See
  [#4408](https://github.com/commercialhaskell/stack/issues/4408)
* When the Cabal spec version is newer than the global Cabal version, build
  against the snapshot's Cabal library. See
  [#4488](https://github.com/commercialhaskell/stack/issues/4488)
* Docker: fix detection of expected subprocess failures.  This fixes
  downloading a compatible `stack` executable  when the host `stack` is not
  compatible with the Docker image (on Linux), and doesn't show an unnecessary
  extra error when the in-container re-exec'ed `stack` exits with failure.
* The `stack ghci` command's `--ghc-options` flag now parses multiple options.
  See [#3315](https://github.com/commercialhaskell/stack/issues/3315).
