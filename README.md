# Chiffrement par substitution et Algorithme Génétique

Pour déchiffrer les chiffrements par substitutions, on utilise une méthode génétique.

D'abord, il faut pouvoir mesurer la plausibilité d'une permutation du texte : à quel point elle se rapproche de la permutation déchiffrant le texte. (Par permutation on entend permutation de l'alphabet appliquée au texte).

Ensuite, on utilise une méthode évolutive pour explorer l'espace des permutations possibles. On adopte une telle méthode car elle permet souvent d'éviter les extrêmums locaux.

Cette méthode est la suivante :
- On part avec une génération de permutations (une liste de permutations du texte)
- On génère un certain nombre d'enfants pour chaque permutation dans la génération
- On sélectionne les enfants les plus plausibles
- On recommence

# Génération des enfants

Pour générer les enfants d'une permutation ![formula](https://render.githubusercontent.com/render/math?math=\tau), on compose par des transpositions.
Ces transpositions sont générées par un mélange de Knuth qui est équiprobable.

# Sélection des enfants

La sélection des enfants qui resteront à la prochaine génération se fait par leur plausibilité. On les trie par plausibilité et on ne prend que les `n` meilleurs.

