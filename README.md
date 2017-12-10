# Puppetry

## Puppet-master

Contains the webserver and the client code.

### Protocol:

```
{ "back":
  [ { "lamp": 2:
    , "color":
        { "red" : 123
        , "blue" : 23
        , "green" : 123
        # , "phase" : 12 exists but is currently unused
        }
  , { "lamp": 12
    , "color":
        { "red" : 12
        # -- missing values are assumed 0
        }
    }
  ]
, "middle" : ...
, "front" : ...
, "left" : ...
, "right" : ...
, "proscenium" : ...
}
```


## Puppet-lights

Contains the arduino code, that manages the theater's lights.
