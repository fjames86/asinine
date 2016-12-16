# asinine
Common Lisp ASN.1 parser. Only supports DER encoding.

## 1. Introduction
ASN.1 is an abstract message description language, for which multiple *encoding rules* can be definied. These
rules specify the way to convert instances of the abstract types described by an ASN.1 definition into concrete
realizations, in either binary or textual forms.

The most common encoding rule is the so called Basic Encoding Rules (BER), which are somewhat complicated because they offer
several options in how to represent objects. As a result, two different BER encodings of the same object can result in different
binary values due to different use of these options.

Many applications, notably in scenarios involving security and cryptography, require a unique encoding of objects. The Distinguished
Encoding Rules (DER) are a subset of BER which remove the various options and specify a single way of encoding the ASN.1 structures.

There are several other encoding rules, including some which encode to textual representations.

This library only supports DER.

## 2. Usage
The ASNINE package defines functions for encoding/decoding the primitive types and definine "sequences", which is the ASN.1 name for
structures. Users should use these to write functions with the following signatures:

* encoder: (stream object) encodes the object to the stream, no meaningful return value.
* decoder: (stream) decodes the object from the stream returning the object.

## 2. Protocol compiler
Because it is somewhat awkward to type all this in, a protocol compiler is provided by the "ASNINE-PARSER" system. This will read
an ASN.1 definition and generate Lisp code to encode/decode all of its assigned type definitions.

Because it just generates Lisp code you are then free to treat it as a template and tweak the function definitions, e.g.
to convert flags into a list of keywords.

## 3. Examples
The Kerberos V5 specifications is provided as an example.

```
(asinine-parser:compile-definition "krb5.asn")
```

Should produce as file "krb5.lisp".

## 4. TODO
[ ] Support sequences with mixtures of tagged and non-tagged elements
[ ] Support CHOICE types


## 5. License
Licensed under the terms of the MIT license.

Frank James
July 2015.

