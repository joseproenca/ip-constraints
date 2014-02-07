Creating a standalone jar file
==============================

The ant script ```build-reopp-scp.xml``` assumes that all binaries have been compiled in the folder ```../out```, used by default in eclipse. Also, it will assume some eclipse plugins are installed (junit and scala), using the folder conventions in OSX, and imports the needed libraries. If necessary, this file can be easily modified to adapt to a custom system.

If all the requirements are met, the standalone jar file can be built by typing:
```
ant -f build-reopp-scp.xml
```