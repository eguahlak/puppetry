# Puppetry

## Puppet-master

Contains the webserver and the client code.

## Puppet-lights

Contains the arduino code, that manages the theater's lights.

Idé til protokol:

```
{ "B" :
  [ { "lamp" : 2, "color" : [255, 240, 0] }
  , { "lamp" : 12, "color" : [0, 0, 134]}
  ]
, "M" : ...
, "F" : ...
, "L" : ...
, "R" : ...
, "P" : ...
}
```
