% Génération dynamique de sites statiques
% Clément Delafargue
% 16 avril 2013

# Un brin d'histoire

## Pages perso en HTML statique

Quelques pages, pas DRY, contenu en HTML

Lourd, mélange contenu / présentation

## CMSs, CMSs everywhere

Wordpress, Spip

## Le renouveau

Markdown => contenu plus léger

Templating => keep it DRY

Github pages => easy to set up

# Pourquoi ça poutre

## Workflow

Full text -> developer friendly (vim, git, make, …)

Contributions: pull requests

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

Ruby: jekyll, nanoc

Python: pelican, frozen flask

Node: blacksmith

Haskell: hakyll, gitit, yst

## Hakyll

- Multi-plateformes
- pandoc
- flexible

# À l'attaque

## Installation

### Haskell platform (ghc + cabal)

Installe le compilo et l'outil de build / gestion de dépendances

Dispo sur GNU/Linux, MacOS et Windows

    aptitude install haskell-platform
    brew install haskell-platform

Pour windows, Mingw + MSYS en plus

------------------------------

### Hakyll

    cabal update
    cabal install hakyll

(peut prendre un peu de temps)

## Warming up

    hakyll-init blog
    cd blog
    ghc --make site.hs
    ./site preview

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
publish
    git add .
    git stash save
    git checkout publish || git checkout --orphan publish
    find . -maxdepth 1 ! -name '.' ! -name '.git*' ! -name '_site' \
        -exec rm -rf {} +
    find _site -maxdepth 1 -exec mv {} . \;
    rmdir _site
    git add -A && git commit -m "Publish" || true
    git push -f git+ssh://my-remote publish:master
    git checkout master
    git clean -fdx
    git stash pop || true
```

## Makefile - like a boss

    make clean
    make preview
    make publish

## Playing with Hakyll

### Concepts de base

Définition d'un ensemble de pipelines `Input File -> Output File`

Templates "purs" (pas de logique, juste des points d'injection)

------------------------------

### Structure de base

```haskell
main :: IO ()
main = hakyll $ do
    rule_1
    …
    rule_n

…
helpers
```

------------------------------

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

### Pages statiques

```haskell
match (fromList [ "about.rst" , "contact.markdown" ]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
```

------------------------------

### Templates

Pas de `route` -> pas exposé dans le site généré

```haskell
match "templates/*" $ compile templateCompiler
```

------------------------------

### Ça reste du code

```haskell
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
```

------------------------------

### Posts


```haskell
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```

------------------------------


### Archive

On peut créer des pages ex nihilo

```haskell
create ["archive.html"] $ do
    route idRoute
    compile $ do
        let archiveCtx =
                field "posts" (\_ -> postList recentFirst) `mappend`
                constField "title" "Archives"              `mappend`
                defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls
```


------------------------------

### Index

```haskell
match "index.html" $ do
    route idRoute
    compile $ do
        let indexCtx = field "posts" $ \_ ->
                            postList $ fmap (take 3) . recentFirst

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
```

## Concepts de base

### Item

Paire `(identifiant, contenu)`


------------------------------

### Compilateur

Permet de transformer un `Item`, en gérant les dépendances.

Par exemple : `pandocCompiler`

Monadique => traduit la nature séquentielle des compilations

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

`mappend` permet de combiner deux contextes (`Context` est un monoide)

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
    match $ fromGlob (lang ++ "/posts/*") do
        route $ setHtmlLang
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
)
```

------------------------------

### Disqus

Ajouter Disqus à ses articles de blog

`templates/disqus.html`

```html
$body$
<!-- Disqus stuff-->
<section>
<div id="disqus_thread"></div>
<script type="text/javascript">
  /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
  var disqus_shortname = 'blogclementd',
      disqus_url = window.location.href
        .split('/')
        .splice(0,3)
        .join("/")+'$url$';
  /* * * DON'T EDIT BELOW THIS LINE * * */
  (function() {
      var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
      dsq.src ='http://' + disqus_shortname + '.disqus.com/embed.js';
      (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
  })();
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
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/disqus.html"  postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
```
