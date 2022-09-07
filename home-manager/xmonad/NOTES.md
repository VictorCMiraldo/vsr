# HiDPI display notes:

## LightDM

If LightDM already sets `xft-dpi`, you're in trouble. Try setting the following settings in
`/etc/lightdm/lightdm-gtk-greeter.conf` or similar:

```
[greeter]
[...]
xft-dpi=261
[...]
```

Check the [arch wiki](https://wiki.archlinux.org/title/User:Ctag/HiDPI) for more.
