# Galaktion v0.2.0

<br>

## A Few Words
Galaktion v0.2.0 is a simple general-purpose scripting imperative programming language but with focus on linked XML Data management, built to support special-purpose concepts. It is the main part of its author's Thesis.
<br><br>
This version is entirely implemented in Python.<br>
It works well, but it has many bugs and is not focused on performance. These will be addressed in future versions.
<br><br>
It uses XPath 1.0 expressions, a core feature of the language, to retrieve data from XML files and combines them with the arrow (->) to navigate through relationally linked XML files.
<br><br>
Galaktion is named after saint Galaktion (November 5th).

<br>

## Usage
A standalone installer for the language is not ready yet.
<br><br>
Therefore, to use Galaktion, you have to clone this repository on your system and execute the file src/Galaktion_parser.py with the file containing your source Galaktion code passed as command line argument to it.<br>
You must have Python installed.
<br><br>
Galaktion files have the extension __.gal__.<br>
For writing Galaktion code inside a .gal file, a simple couloring extension for Visual Stusio Code is availble on this page. You can download it and install it into your VS Code extensions.
<br><br>
Future plans include the implementation of a Language Server Protocol.

<br>

## Simple Program
This simple Galaktion program declares the `$myFile` variable of `xmlfile` type to represent the my_file.xml file of the Current Working Directory.<br>
Then it uses an XPath expression to get the text of the node `World` under the node `Hello` from this file and print it to the console.
```
xmlfile $myFile := './my_file.xml'

from $myFile print //Hello/World/text()
```
<br>
More expamples are available in the examples folder.
