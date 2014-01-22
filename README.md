## FenixEdu SDK for Common Lisp

### Requirements

The common lisp SDK has several requirements. It is recommended that you use quicklisp to load the project as it will download and install them automatically.

+ Drakma
+ py-configparser
+ jsown    

### Instalation

As it is recommended that you use quicklisp this is how you load the SDK with it:

First you download and setup quicklisp, if you haven't done it yet.

Then you load the asd file `fenixedu-commonlisp-sdk.asd`.

After this, you load the project through quicklisp: 

      (ql:quickload :fenixedu-commonlisp-sdk)

This should fetch, install and load all the dependencies and the SDK as well.

After loading the SDK and it's dependencies, you have to set your app details.

Copy file fenixedu.sample.ini to a new one called fenixedu.ini 
     cp fenixedu.sample.ini fenixedu.ini

Move it to your project's root 
     mv fenixedu.ini project_dir 

Edit ```fenixedu.ini``` file according to your app info.

### Usage

After this is set, you can start the sdk.

You can accomplish this by calling:
    
    (cl-fenix:startup)

After this you can try the SDK by making a public call:
      
    (cl-fenix:get-about)

which should return you the information about the institution.

