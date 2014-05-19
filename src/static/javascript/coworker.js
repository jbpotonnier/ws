var Coworker = can.Model.extend(
    {
	create: function(attrs){
	    return $.ajax({
		type: "POST",
		url: '/coworkers',
		data: JSON.stringify(attrs),
		contentType:"application/json"
	    });
	},
	findAll: 'GET /coworkers',
	findOne: 'GET /coworkers/{id}',
	update: 'PUT /coworkers/{id}',
	destroy: 'DELETE /coworkers/{id}' 
    },
    {});

var Coworkers = can.Control.extend({
    init: function (element, options) {
        var that = this;
        Coworker.findAll({}, function (coworkers) {
            that.coworkers = coworkers;
            that.element.html(can.view('coworkersMustache', {coworkers: coworkers}));
        });
    },

    "{Coworker} created": function (Coworker, ev, coworker) {
        this.coworkers.push(coworker);
    }
});


var AddCoworker = can.Control.extend({
    init: function (element, options) {
        this.$form = this.element.find('form');
        this.$form.easyModal({
            top: 100,
            overlayOpacity: 0.5,
            overlayColor: "#000"});
    },

    '.add click': function (el, evt) {
        this.$form.trigger('openModal');
    },

    '.save click': function(el, evt) {
	evt.preventDefault();
	var that = this;
	var coworker = new Coworker({
	    firstname: that.$form.find('[name=firstname]').val(),
	    lastname: that.$form.find('[name=lastname]').val(),
            email: that.$form.find('[name=email]').val()
	});
	coworker.save(function(){
	    that.$form.trigger('closeModal');
	});
    }
});

new Coworkers('#coworkers', {});
new AddCoworker('#add-coworker', {});
