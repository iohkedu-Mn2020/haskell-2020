# How can we have private branch only for teachers?

This is is a very interesting question, for example you might need to:

- Keep notes that are not complete yet before sharing with all the students.
- Maybe have a test, and only publish it when is the time for the exam.

However, turns out is *not possible* to have some branches visible for some users and not others.


What *is* possible is to have different repositories visible by different sets of persons.
With that then we can change the original question to:

## How to have a private repository for teachers, and a private repository for students?

The idea is that we have the Teachers Repository.
And we have the Students repository which is a *fork* of the teachers repostory.
Now in the teachers repo there is a master branch.
Everything committed to the *teachers-master* branch  is automatically merged to the *students* _repository_  .
  <script src="https://cdn.jsdelivr.net/npm/@gitgraph/js"></script>
  <!-- DOM element in which we'll mount our graph -->
  <div id="graph-container"></div>

  <!-- Use the `GitgraphJS` global variable to create your graph -->
  <script>

// Get the graph container HTML element.
const graphContainer = document.getElementById("graph-container");

// Instantiate the graph.
const gitgraph = GitgraphJS.createGitgraph(graphContainer);

const teacher = gitgraph.branch("teacher-master");
teacher.commit("Create new instance");
teacher.commit("Add lectures in PDF");

const dev = teacher.branch("teacher-New-Exercise");
const exam = teacher.branch("teacher-New-Exam");

const student = gitgraph.branch({
  name: "student-master",
  style: {
    template: "metro"
  }
});
teacher.merge(student, "publish for students");

teacher.commit("Add Exercices");
teacher.merge(student, "publish for students");

teacher.commit("Add latex");
teacher.merge(student, "publish for students");



dev.commit("Create a new exercise");
dev.commit("add tests and experiment");
dev.commit("we can try many different things")
dev.merge(teacher, "when ready merge to teacher-master")

teacher.commit("Since it's in master it gets published automatically");
teacher.merge(student, "publish for students");


  </script>

## Feature Branches

This organization opens the possibility for *feature branches* on the teachers repository.
For example a teacher can start to create a new Lecture in a feature branch,
then merge it to *teachers-master* and then get the new lesson automatically published to the *students* repo.

The same principle applies to Exams.
An exam is "hidden" in an exam branch,
that once it's time for publishing get's merged in *teachers-master* and from there automatically published to *stuents* repo.



Now this repository organization allow
