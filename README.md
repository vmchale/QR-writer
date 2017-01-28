# QR Imager Library
![alt text](https://travis-ci.org/vmchale/QR-writer.svg?branch=master "build status")

This is a library to generate `.png` files from QR codes.

## Dependencies
The library depends on the C library [libqrencode](https://github.com/fukuchi/libqrencode) which you will need to install separately, as well as the C library `Zbar` from [here](https://github.com/ZBar/ZBar). You should also be able to get them from your distro. 

## Library
The library can be used via the exported functions `createQRCode`, `byteStringToQR` and `readQRString`, plus their signed counterparts. The first two export to ".png" while the third can be used on any image format `Zbar` supports.

The functions `bsToImg` and `objToImg` output JuicyPixels images for your further manipulation.

## Executable

### Installation
For building haskell, the best tool is currently [http://haskellstack.org](stack). Install it, and then type

```
stack install --install-ghc
```

in the appropriate directory, and it will be installed to your path. 

### Use

Compiling will generate an executable called `qrpipe` which reads from `stdin` and outputs a file as the second argument, e.g.

```
echo 'My name is: Vanessa" | qrpipe write -v "nametag.png"
```

To then read the nametag:

```
qrpipe read "nametag.png"
```

