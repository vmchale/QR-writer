#QR Imager Library
This is a library to generate `.png` files from QR codes.

##Dependencies
The library depends on the C library [https://github.com/fukuchi/libqrencode](libqrencode) which you will need to install separately.

##Usage
The library exports main functions - `createQRCode` and `byteStringToQR` - and their secured/signed versions. The first takes any object that is an instance of `ToJSON` and writes an image to file, while the second takes a (strict) bytestring and writes it to file.

##Executable

###Installation
For building haskell, the best tool is currently [http://haskellstack.org](stack). Install it, and then type

```
stack install --install-ghc
```

in the appropriate directory, and it will be installed on your path. 

###Use

Compiling will generate an executable called `QRPipe` which reads from `stdin` and outputs a file as the second argument, e.g.

```echo 'My name is:" | qrpipe "nametag.png"```
