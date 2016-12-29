# QR Imager Library
This is a library to generate `.png` files from QR codes.

## Dependencies
The library depends on the C library [https://github.com/fukuchi/libqrencode](libqrencode) which you will need to install separately, as well as the C library `Zbar` from [here](https://github.com/ZBar/ZBar). You should also be able to get them from your distro. 

## Usage
The library exports three main functions - `createQRCode`, `readQRString`, and `byteStringToQR` - and their secured/signed versions. The first takes any object that is an instance of `ToJSON` and writes an image to file, while the second takes filepath pointing to an image and returns the text in the QR code. The third takes a (strict) bytestring and writes it to file.

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

## Library
The library can be used via the exported functions `createQRCode`, `byteStringToQR` and `readQRString`, plus their signed counterparts. 
