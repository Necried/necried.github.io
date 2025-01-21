# This is a simplified version of the original deploy.sh,
# as I no longer require keeping separete versions of my website
# anymore, since I am committing to using Hakyll to maintain
# my static pages.

# Verify correct branch
git checkout master

# Build new files
cd docs
stack exec docs clean
stack exec docs build
cd ..

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin master:master
