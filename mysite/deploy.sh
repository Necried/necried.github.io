# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout hakyll-develop

# Build new files
stack exec mysite clean
stack exec mysite build

# Get previous files
git fetch --all
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a _site/. ..

# Commit
echo ">>>>>> Publishing"
git add -A
git commit -m "Publish."

# Push
echo ">>>>>> Pushing"
git push origin master:master

# Restoration
echo ">>>>>> Restoration"
git checkout hakyll-develop
git branch -D master
git stash pop
