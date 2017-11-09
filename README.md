# Elm Resume

The idea behind this project is simple. At times you need to build a new resume. Since last time you did it many things have changed, you don't want to use Word anymore, the template you used looks old, LinkedIn export is really not helping much ... but one thing didn't really change, your previous experiences. You might want to trim them and update them but generally not change them that much.

The solution here is to focus on the data in your resume. You describe your experiences in a JSON file and you focus on keeping that updated. Commit your changes on Github and your resume is automatically updated.

## How does it work

There are two main pieces to Elm Resume.

1. This project contains all the code to interpret and display your JSON data.
1. A secondary project where you store and distribute your JSON data. This doesn't strictly have to be a separate project nor does it have to be stored on Github but I find it easier that way.

## Usage

1. Create a new Github project. It can be private or public, it doesn't matter. It also doesn't matter how you name it but for reference let's call it `elm-resume-data`.
1. Create a `docs/data.json` file in `elm-resume-data` repository.
1. Go to the project settings on Github and make sure that the option for Github Pages is on and `master branch /docs folder` is selected. No need to choose a theme.
1. At this point you should have a publicly accessible resume JSON at a location similar to this: [https://fponticelli.github.io/elm-resume-data/data.json](https://fponticelli.github.io/elm-resume-data/data.json) (you can use this as a reference).
1. Go back to this project and fork it on your own Github account (use the top right button on this page).
1. Clone that fork to your own local machine. You can add a new remote if you want to easily pull in new changes from this repo. The command is `git add remote upstream git@github.com:elm-resume/elm-resume.git`.
1. Open the `docs/index.html` file in your editor of choice and change its `api_base_url` field to point to your own JSON data published earlier.
1. Go to your forked project settings and repeat point `3.`. Now your resume is available online for everybody to see.
1. Bonus Point. If you are not satisfied with the default style of the resume you can provide your own CSS. Just replace the reference in your `index.html` to point to something else. I suggest to put the new style file in your `elm-resume-data` file. Mine looks like this: `https://resume.weblob.net`.
