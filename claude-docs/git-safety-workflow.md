# Git Safety Workflow for Claude Sessions

## Before Starting with Claude

1. **Check status**: `git status`
2. **Stage changes**: `git add .`  
3. **Create snapshot**: `git commit -m "Before Claude session - safety snapshot"`
4. **Optional - Push to GitHub**: `git push origin [branch-name]`

## After Claude Session

1. **Review changes**: `git status` and `git diff`
2. **Commit Claude's work**: `git commit -m "After Claude session - [describe what was done]"`
3. **Push if desired**: `git push origin [branch-name]`

## Emergency Recovery

If something goes wrong, revert to your last commit:
```bash
git reset --hard HEAD~1
```

## Current Branch
You're working on: `refactor-plotting-code`