# ENS projet programmation 

Ce projet consiste à implémenter un compilateur C-- vers LC-3 (voir sujet [projet-ocaml-sujet-synthese.pdf])

## Utilisation du compilateur

Commencer par se rendre dans le dossier `src`.
```
cd src
```

Compiler le projet.

```
make
```

Ecrire un fichier `name.c` dans les conventions C-- décrites dans le sujet [projet-ocaml-sujet-synthese.pdf]. Compiler le fichier par la commande suivante.

```
./ccomp --debug name.c
```

Un fichier `name.s` contenant du LC-3 devrait être produit. Pour tester l'assembleur produit, se rendre sur le site [LC3-tutor](http://lc3tutor.org), cliquer sur le bouton `Assemble`, copier coller le code, puis à nouveau, cliquer sur `Assemble`. Enfin executer en cliquant sur `Run`. A la fin de l'execution, le résultat de retour de la fonction `main` est le seul élément de la pile, à l'adresse `xFFDF`.

## Visulatlisation de l'AST

Lors de l'appel

```
./ccomp --debug name.c
```
un fichier `name_ast.dot` est aussi généré. En executant la commande suivante il permet de générer le fichier `name_ast.dot.png` pour visualiser l'arbre syntaxique : 

```
dot -v -Tpng -O name_ast.dot
```

## Structure des tests

Dans le dossier `src` se trouvent 17 fichiers de test C--. Les 10 premiers échouent à la compilation et testent les différentes fonctionnalités de [src/ctyping.ml]. Les 7 suivants compilent et testent differentes fonctionnalités de [src/compile.ml].
