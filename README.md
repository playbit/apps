A collection of Playbit apps

To build & run these, [install Playbit](https://playbit.app/) and then in a terminal:

```
export PATH="$(mdfind kMDItemCFBundleIdentifier=='playbit.engine')/Contents/MacOS:$PATH"
git clone https://github.com/playbit/apps.git pb-apps && cd pb-apps
cd football &&
pb run
```

Click the "Copy Session Link" button in the top right corner of a window to invite a friend to join your session.

## License

You can copy, modify, and share these apps without attribution under the [MIT-0 license](LICENSE.txt).
