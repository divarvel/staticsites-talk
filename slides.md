% Static site generation with Hakyll
% Clément Delafargue
% scala.io - 2013-10-25

# Some history

## Static HTML personal homepages

Few pages, not DRY, content directly in HTML

Heavy, mixes up content and presentation

## CMSs, CMSs everywhere

Wordpress, dotclear, Spip, …

## A new hope

Markdown => slick content

Templating => keep it DRY

Github pages => easy to set up

# Why it's awesome

## Workflow

Full text -> **developer friendly** (vim, git, make, …)

Contributions: pull requests

Publication: scp, git, dropbox, …

## Bonus

- cheap hosting (GH pages, S3)
- secure
- fast

## Limits

- rigid
- not for everyone
- not dynamic

## … and how to avoid them

- some tools are really flexible
- education
- external services


## The tool for the job

**Ruby**: jekyll, nanoc, …

**Python**: pelican, frozen flask, …

**Node**: blacksmith, …

**Haskell**: hakyll, gitit, yst, …

## Hakyll

- Haskell
- Multi-plateform
- uses pandoc
- **extremely** flexible

## Hakyll

<http://jaspervdj.be/hakyll>

Library + eDSL => build your own generator

# Let's go

## Installation

    cabal install hakyll


## Warming up

Blog + static pages

    hakyll-init blog
    cd blog
    ghc --make site.hs # compiles the generator
    ./site preview # HTTP server + reload

## Makefile

Let's improve the workflow

- recompiles the generator when the source code is modified
- cleans the cache
- git-push publication
- …

## Makefile

```Makefile
all: build

build: site
	./site build

site: site.hs
	ghc --make site.hs
	./site clean

preview: site
	./site preview

clean: site
	./site clean

check: site
    ./site check
```

## Makefile - publish

```Makefile
publish: build
    git add .
    git stash save
    git checkout publish || git checkout --orphan publish
    find . -maxdepth 1 ! -name '.' ! -name '.git*' ! -name '_site' \
        -exec rm -rf {} +
    find _site -maxdepth 1 -exec mv {} . \;
    rmdir _site
    -git add -A && git commit -m "Publish"
    git push -f git+ssh://my-remote publish:master
    git checkout master
    git clean -fdx
    -git stash pop
```

## Makefile - like a boss

    make clean
    make preview
    make check # detect broken links
    make publish

## Playing with Hakyll

### Base concepts

Pipelines `Input -> Output`

Matching -> Route -> Compilation -> Template injection

------------------------------

## Playing with Hakyll
### Base structure

```haskell
main :: IO ()
main = hakyll $ do
  rule_1
  …
  rule_n


helpers
```

------------------------------

## Playing with Hakyll
### Static Assets

```haskell
match "images/*" $ do
  route   idRoute
  compile copyFileCompiler

match "css/*" $ do
  route   idRoute
  compile compressCssCompiler
```

------------------------------

## Playing with Hakyll
### Static pages

```haskell
match (fromList [ "about.rst" , "contact.markdown" ]) $ do
  route   $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "tpl/default.html" defaultContext
    >>= relativizeUrls
```

------------------------------

## Playing with Hakyll
### Templates

No `route` -> not exposed in the generated site

```haskell
match "tpl/*" $ compile templateCompiler
```

------------------------------

## Playing with Hakyll
### Helpers

```haskell
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

postList :: ([Item String] -> Compiler [Item String])
         -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "tpl/post-item.html"
  list    <- applyTemplateList itemTpl postCtx posts
  return list
```

------------------------------

## Playing with Hakyll
### Posts


```haskell
match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "tpl/post.html"    postCtx
    >>= loadAndApplyTemplate "tpl/default.html" postCtx
    >>= relativizeUrls
```

------------------------------


## Playing with Hakyll
### Archive

Create pages from scratch

```haskell
create ["archive.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
                listField "posts" postCtx (return posts) `mappend`
                constField "title" "Archives"            `mappend`
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls
```


------------------------------

## Playing with Hakyll
### Index

```haskell
match "index.html" $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
                listField "posts" postCtx (return posts) `mappend`
                constField "title" "Home"                `mappend`
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
```

## Base concepts

### Item

`(identifiant, contenu)` pair


------------------------------

### Compiler

Transforms an `Item`, handles the dependencies

eg : `pandocCompiler`

*Monadic* => because compilations are sequential

------------------------------

### Compiler

Arbitrary action

```haskell
match "assets/css/*.less" $ do
  route   $ setExtension "css"
  compile $ getResourceString >>=
    withItemBody (unixFilter "lessc" ["-","--yui-compress","-O2"])
```

------------------------------

### Context

Used to inject data in a template

eg, `defaultContext` injects:

 - metadata (title, author, …)
 - body

------------------------------


### Context

In the file:

```
---
title: Foo bar baz
---

My awesome content
```

In the template:

```html
    <article>
        <h1>$title$</h1>
        $body$
    </article>
```

------------------------------

```haskell
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext
```

`postCtx` extracts the date from the file name and injects it in the template.

`mappend` allows to combine two contexts (`Context` forms a *monoid*)

------------------------------

Roll your own context

 - function `Item a -> Compiler String`
 - static data
 - functions  (`$func arg$`)
 - date
 - …

## Common patterns

### i18n (sort of)

Content in `/en` and `/fr` directories

Shared templates

Default lang at root


------------------------------

### i18n (sort of)

```haskell
langs = ["fr", "en"]
defaultLang = "fr"

-- Removes "/fr"
langRoute = gsubRoute (defaultLang ++ "/") (const "")
setHtmlLang = langRoute `composeRoutes` (setExtension "html")
```

------------------------------

### i18n (sort of)

In the routes:

```haskell
forM_ langs (\lang ->
  match $ (fromGlob $ lang ++ "/posts/*") do
    route setHtmlLang
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "tpl/post.html"    postCtx
      >>= loadAndApplyTemplate "tpl/default.html" postCtx
      >>= relativizeUrls
)
```

------------------------------

### Disqus

Add disqus

`tpl/disqus.html`

```html
$body$
<section class="disqus">
<script type="text/javascript">
var page_url = "$url$";
<!-- Disqus stuff  -->
</script>
</section>
```

------------------------------

### Disqus

Modify compilation pipeline

```haskell
match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "tpl/post.html"    postCtx
    >>= loadAndApplyTemplate "tpl/disqus.html"  postCtx
    >>= loadAndApplyTemplate "tpl/default.html" postCtx
    >>= relativizeUrls
```

## Let's go farther

GUI with prose.io

<http://prose.io/>

Tags for blog articles

<http://jaspervdj.be/hakyll/reference/Hakyll-Web-Tags.html>


## Let's go farther

Single page site:

<https://github.com/divarvel/hakyll-single-page-test>

## Let's go farther


scala.io: i18n, factoring, inter-pages dependencies, ICS generation, reusable
blocks

<https://github.com/scalaio/web>

<http://blog.clement.delafargue.name/posts/2013-04-03-web2day-powered-by-hakyll-part-1.html>

## Bisous

Source code

<https://github.com/divarvel/staticsites-talk>

