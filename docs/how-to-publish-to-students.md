# How to publish to information for students

## Summary

1. In order to have one repo for teachers and one for students.
1. As a teacher create a *feature branch* in:
   [teachers-haskell-and-cryptocurrencies](https://github.com/iohkedu/teachers-haskell-and-cryptocurrencies)
1. When ready merge your branch to master
1. It will be automatically published to students repo:
   [haskell-mongolia-2020](https://github.com/iohkedu/haskell-mongolia-2020)

Now for the long explanation:

## How can we have a private branch only for teachers?

One might need private branch to:

- Keep notes that are not complete, before sharing with all the students.
- Have a test, and only publish it when is the time for the exam.

However, turns out is _not possible_ to have some
branches visible to teachers but not for students.

What _is_ possible is to have different repositories visible
by different sets of persons.

With that then we can change the original question to:

## How can we have a private /repository/ for teachers, and a another for students?

The idea is that we have the Teachers Repository.
And we have the Students repository which is a _fork_ of the teachers repostory.
Now in the teachers repo there is a master branch.
Everything committed to the _teachers-master_ branch is
automatically merged to the _students_ _repository_ .

In this diagram the dots in:

- blue are the teachers master repository
- purple is the students repository which automatically gets updated with new content
- green is a feature branch, work in process in the teachers repository.

![Autopublish from teachers to students](img/Autopublishing.png)

## Feature Branches

This organization opens the possibility for _feature branches_ on the teachers repository.
For example a teacher can start to create a new Lecture in a feature branch,
then merge it to _teachers-master_ and then get the new lesson automatically
published to the _students_ repo.

The same principle applies to Exams.
An exam is "hidden" in an exam branch,
that once it's time for publishing get's merged in _teachers-master_
and from there automatically published to _stuents_ repo.

A teacher will spend most of the time in a _feature branch_ and only when is
ready to share the documnet with the students will _merge to master_ the branch
and from there it will be automatically merged to the students repostiory.
