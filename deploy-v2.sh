# This is a simplified version of the original deploy.sh,
# as I no longer require keeping separete versions of my website
# anymore, since I am committing to using Hakyll to maintain
# my static pages.

# Verify correct branch
git checkout master

# Build new files
stack exec myblog clean
stack exec myblog build

# Commit
git add -A
git commit -m "Publish."

# Push
git push origin master:master
