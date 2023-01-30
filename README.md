# TIPE

## Titre
Détection des panneaux de signalisation en les extrayant d'une image

Détection et extraction des panneaux de signalisation d'une image

### Mots clés
Français : Conduite automatisée, Filtrage d'une image, Reconnaissance de formes, Analyse d'image, Efficacité du traitement
Anglais :  Automated driving, Image filtering, Shape recognition, Image analysis, Processing efficiency

### Positionnements théoriques
Informatique (Informatique pratique), Informatique (Informatique théorique), Sciences industrielles (Traitement du signal)

### Bibliographie commentée
Pour arriver à extraire de façon optimale les contours des objets dans une image, nous allons utiliser le filtre de Canny. Ce filtre, dont la théorie est démontrée par Canny [1], va permettre à l'aide de gradients d'intensité de détecter les ruptures de contraste dans l'image. Pour concevoir cet algorithme, il faut au préalable traiter l'image. En effet, lors d'une prise en photo de nuit et avec la vitesse d'obturation du capteur, la photo peut se retrouver bruitée. Pour filtrer l'image, on applique un flou Gaussien en passant chaque pixel par une matrice de convolution [2]. 
Ensuite on détermine les gradients associés à chaque pixel, les gradients les plus élevés représentent ainsi un dénivelé de contraste donc potentiellement le bord d'un objet. En appliquant les formules du modèle continu des contours [3], on calcule les gradients associés et on détermine ainsi les contours. On peut aussi, en changeant de système de couleurs, isoler des nuances intéressantes dans l’image [4]. On pourra alors combiner ces deux méthodes pour ne conserver que les zones d'intérêt et éliminer la plupart des “faux positifs” après analyse. 
Une fois le nombre de points d'intérêt considérablement réduit, nous pouvons appliquer sur l'image une transformée de Hough pour détecter les formes remarquables [5]. Ainsi on pourra identifier les droites présentes dans l'image et les  cercles. Permettant donc l’identification des rectangles, triangles et cercles donc les principales formes de panneaux. 
Enfin pour tester mes algorithmes j'utilise une base d’images de panneaux de signalisations allemands [6]. Ceci sont très semblables aux panneaux français donc conviennent parfaitement pour les tests puis une application de l'algorithme aux routes françaises.

### Problématique retenue
Comment identifier dans une photo sur la route, les différents panneaux de signalisation ? 
Comment le faire de façon efficace ? Comment les extraire en les situant dans le contexte de l'image ?

### Quelle est votre motivation pour le choix du sujet?
Pour le développement de véhicules avec un niveau d'autonomie plus ou moins élevé, l'étude du contexte environnant est un enjeu majeur. Ainsi nous avons souhaité voir comment faire pour extraire les informations d'une image de la route de manière rapide et juste permettant au véhicule d'assurer entre autres sa sécurité et celles des autres usagers.

### En quoi votre étude s'inscrit-elle dans le thème de l'année ? 
Les véhicules autonomes étant voués à être les nouveaux vecteurs de transports citadins. Le developpement de ces véhicule est alors en lien direct avec la ville.

### Objectifs du TIPE
A partir d'une image issue de la route, nous allons chercher à identifier les éléments permettant au véhicule d'appréhender le contexte de la circulation environnante.
Le but de ce TIPE résidant dans l'extraction optimale des éléments :
- Utiliser premièrement un algorithme efficace pour traiter l'image et garder seulement les éléments importants. En appliquant des filtres et de la détection de contour. (Filtre Canny)
- Chercher après à extraire les formes intéressantes tout en minimisant le taux d'erreur. (Transformée de Hough)
- Contextualiser l'information extraite par son positionnement et de ses informations caractéristiques.
- Combiner l'algorithme de mon partenaire TIPE ainsi que le mien pour aboutir à une analyse complète de l'image.

### Liste de références bibliographiques*
[1] J. Canny, "A Computational Approach to Edge Detection," in IEEE Transactions on Pattern Analysis and Machine Intelligence, vol. PAMI-8, no. 6, pp. 679-698, Nov. 1986, doi: 10.1109/TPAMI.1986.4767851.
https://canvas.stanford.edu/courses/98045/files/4183084/download?verifier=af00LZDbnDaaTurZAWvWhzOtJraLfk8DZsQqwuOy&wrap=1

[2] Colin Leverger - ENSSAT Informatique, Multimédia et Réseaux
Promotion 2017
https://colinleverger.fr/assets/projects/CANNY-COLIN-LEVERGER.pdf

[3] Chapitre 2
La detection des contours dans les images
Chapitre redigée par Henri MAITRE
https://perso.telecom-paristech.fr/bloch/TDI/poly_contours.pdf

[4] Traffic sign recognition based on HOG feature extraction
Song Yucong , Guo Shuqing
https://www.extrica.com/article/22022

[5] Use of the Hough Transformation To Detect Lines and Curves in Pictures
Richard O. Duda and Peter E. Hart Stanford Research Institute, Menlo Park, California
https://www.cse.unr.edu/~bebis/CS474/Handouts/HoughTransformPaper.pdf

[6] J. Stallkamp, M. Schlipsing, J. Salmen and C. Igel, "The German Traffic Sign Recognition Benchmark: A multi-class classification competition," The 2011 International Joint Conference on Neural Networks, San Jose, CA, USA, 2011, pp. 1453-1460, doi: 10.1109/IJCNN.2011.6033395.
https://benchmark.ini.rub.de/gtsdb_dataset.html