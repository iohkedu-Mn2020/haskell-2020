var myUsers = ko.observableArray();
var myFirstName = ko.observable("");
var myLastName = ko.observable("");
var myEMail = ko.observable("");
var myAge = ko.observable(0);

var myViewModel = {

    users: myUsers,
    firstName: myFirstName,
    lastName: myLastName,
    email: myEMail,
    age: myAge,

    refresh: function () {
        console.log("refresh clicked");
        getUsers(function(result) {
            console.log("Result: " + JSON.stringify(result));
            myUsers(result);
        }, function(err) {
            console.log("ERROR in getUsers: " + JSON.stringify(err));
        });
    },

    addUser: function(formElement) {
        console.log("add user clicked");

        var body = {
            uId: [],
            uFirstName: myFirstName(),
            uLastName: myLastName(),
            uEMail: myEMail(),
            uAge: parseInt(myAge())
        };

        console.log("user: " + JSON.stringify(body));

        postUser(
            body,
            function (result) {
                console.log("Result: " + JSON.stringify(result));
                myViewModel.refresh();
            },
            function (err) {
                console.log("ERROR in postUser: " + JSON.stringify(err));
            });

        myFirstName("");
        myLastName("");
        myEMail("");
        myAge(0);
    },

    remove: function(user) {
        console.log("remove clicked on " + JSON.stringify(user));
        deleteUserById(
            user.uId,
            function () {
                myViewModel.refresh();
            },
            function (err) {
                myViewModel.refresh();
            });
    }

};

$(function () {
    ko.applyBindings(myViewModel);
    myViewModel.refresh();
});
