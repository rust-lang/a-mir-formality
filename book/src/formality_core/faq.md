# FAQ and troubleshooting

## Why am I getting errors about undefined references to `crate::FormalityLang`?

The various derive macros need to know what language you are working in.
To figure this out, they reference `crate::FormalityLang`, which you must define.
See the [chapter on defining your language](./lang.md) for more details.
