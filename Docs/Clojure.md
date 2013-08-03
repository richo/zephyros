## Clojure API

#### Running the script manually

1. Download the subtree at https://github.com/sdegutis/zephyros/tree/tcp/libs/zephyros-clj
2. Copy it somewhere outside of Zephyros
3. `cd zephyros-clj`
4. `lein run`

#### Letting Zephyros run it for you

Put this as your auto-launch-command inside Zephyros:
```bash
cd path/to/zephyros-clj && lein run
```

#### Editing the script

* Edit your [src/zephyros/core.clj](../libs/zephyros-clj/src/zephyros/core.clj)
* See your [src/zephyros/api.clj](../libs/zephyros-clj/src/zephyros/api.clj)
