% Génération dynamique de sites statiques
% Clément Delafargue
% 16 avril 2013

# Un brin d'histoire

## Pages perso en HTML statique

Quelques pages, pas DRY, contenu en HTML

Lourd, mélange contenu / présentation

## CMSs, CMSs everywhere

Wordpress, dotclear, Spip, …

## Le renouveau

Markdown => contenu plus léger

Templating => keep it DRY

Github pages => easy to set up

# Pourquoi ça poutre

## Workflow

Full text -> **developer friendly** (vim, git, make, …)

Contributions: pull requests

Publication: scp, git, dropbox, …

## En bonus

- cheap hosting (GH pages, S3)
- secure
- fast

## Tout n'est pas rose…

- rigide
- pas adapté à tout le monde
- pas de composante dynamique

## … mais on peut se débrouiller

- certains outils sont très souples
- un peu d'éducation, ça marche
- utilisation de services externes


## Quels outils choisir ?

**Ruby**: jekyll, nanoc

**Python**: pelican, frozen flask

**Node**: blacksmith

**Haskell**: hakyll, gitit, yst

## Hakyll

- Multi-plateformes
- pandoc
- flexible

## Hakyll

<http://jaspervdj.be/hakyll>

# À l'attaque

## Installation

### Haskell platform (ghc + cabal)

Installe le compilo et l'outil de build / gestion de dépendances

<http://www.haskell.org/platform/> (Gnu/Linux, MacOS, Windows)

Pour windows, Mingw + MSYS en plus

------------------------------

## Installation
### Hakyll

    cabal update
    cabal install hakyll

(peut prendre un peu de temps)

## Warming up

Crée un blog + pages statiques

    hakyll-init blog
    cd blog
    ghc --make site.hs # compile le générateur
    ./site preview # serveur HTTP + reload

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
```

## Makefile - publication

```Makefile
publish:
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

L'utilisation de `make` permet d'avoir un binaire toujours à jour.

    make clean
    make preview
    make publish

## Playing with Hakyll

### Concepts de base

Ensemble de pipelines `Input -> Output`

Matching -> Route -> Compilation -> Injection template

Templates "purs" (pas de logique, juste des points d'injection)

------------------------------

## Playing with Hakyll
### Structure de base

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
### Pages statiques

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

Pas de `route` -> pas exposé dans le site généré

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

On peut créer des pages ex nihilo

```haskell
create ["archive.html"] $ do
  route idRoute
  compile $ do
    let ctx =
      field "posts" (\_ -> postList recentFirst) `mappend`
      constField "title" "Archives"              `mappend`
      defaultContext

    makeItem ""
      >>= loadAndApplyTemplate "tpl/archive.html" ctx
      >>= loadAndApplyTemplate "tpl/default.html" ctx
      >>= relativizeUrls
```


------------------------------

## Playing with Hakyll
### Index

```haskell
match "index.html" $ do
  route idRoute
  compile $ do
    let indexCtx = field "posts" $ \_ ->
                        postList $ fmap (take 3) . recentFirst

    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "tpl/default.html" postCtx
      >>= relativizeUrls
```

## Concepts de base

### Item

Paire `(identifiant, contenu)`


------------------------------

### Compilateur

Permet de transformer un `Item`, en gérant les dépendances.

Par exemple : `pandocCompiler`

*Monadique* => traduit la nature séquentielle des compilations

------------------------------

### Compilateur

On peut y mettre ce qu'on veut. En particulier, du shell

```haskell
match "assets/css/*.less" $ do
  route   $ setExtension "css"
  compile $ getResourceString >>=
    withItemBody (unixFilter "lessc" ["-","--yui-compress","-O2"])
```

------------------------------

### Contexte

Données injectées dans un template.

Par exemple, `defaultContext` injecte :

 - métadonnées (title, author, …)
 - body

------------------------------


### Contexte

Dans le fichier :

```
---
title: Foo bar baz
---

My awesome content
```

Dans le template :

```html
    <article>
        <h1>$title$</h1>
        $body
    </article>
```

------------------------------

```haskell
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext
```

`postCtx` extrait la date du nom de fichier et l'injecte dans le template.

`mappend` permet de combiner deux contextes (`Context` est un *monoide*)

------------------------------

Possibilité de construire ses propres contextes

 - fonction `Item a -> Compiler String`
 - données statiques
 - fonctions  (`$func arg$`)
 - date
 - …

## Patterns courants

### i18n (sort of)

Contenu séparé dans des dossiers `/en` et `/fr`.

Templates en commun

Langue par défaut à la racine du site généré


------------------------------

### i18n (sort of)

```haskell
langs = ["fr", "en"]
defaultLang = "fr"

-- Enlève automatiquement le "/fr" en début d'URL
langRoute = gsubRoute (defaultLang ++ "/") (const "")
setHtmlLang = langRoute `composeRoutes` (setExtension "html")
```

------------------------------

### i18n (sort of)

Dans les routes :

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

Ajouter Disqus à ses articles de blog

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

Modification des règles de compilation

```haskell
match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "tpl/post.html"    postCtx
    >>= loadAndApplyTemplate "tpl/disqus.html"  postCtx
    >>= loadAndApplyTemplate "tpl/default.html" postCtx
    >>= relativizeUrls
```

## On peut aller plus loin

Single page site:

<https://github.com/divarvel/hakyll-single-page-test>

## On peut aller plus loin

Web2day 2013: i18n, factorisation, dépendances inter-pages

<https://github.com/CompanyCampus/web2day2013>

<http://blog.clement.delafargue.name/posts/2013-04-03-web2day-powered-by-hakyll-part-1.html>

## On peut aller plus loin

GUI avec prose.io

<http://prose.io/>
