# manumission
Utility for creating Microsoft HTML Help (chm) files.
## Overview
Microsoft HTML Help files provide a convenient, cross-platform method for displaying program documentation. They can be viewed on all current Windows versions without the need to install a viewer. On Linux and Mac OS, you'll need to install xCHM.

manumission creates chm files using html which you provide. In addition, it now creates default contents and index files (which appear in the Contents and Index tabs of the Help viewer).

Currently, manumission's output is uncompressed only.

## Installation
You'll need [Bigloo Scheme](https://www-sop.inria.fr/mimosa/fp/Bigloo/) to compile manumission.

Clone this repository, then
```
cd manumission
make
```

If everything goes well, you'll get 'sample.chm', which is produced from the 'sample/' directory.
