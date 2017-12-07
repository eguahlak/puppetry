# Haskel backend with Elm frontend using WebSockets

To import packages (if necessary):

```
> elm package install
> stack install --dependencies-only
```

The backend:
```
> stack runhaskell ./src/Main.hs
```

In another window, the frontend:

```
> elm reactor
```
