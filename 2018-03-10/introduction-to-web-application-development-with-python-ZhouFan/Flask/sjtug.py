from flask import Flask, request, render_template, abort

app = Flask(__name__)

posts = ['Haskell小课堂 01',
        'SJTUG例行分享：系统性能的测量与分析',
        'SJTUG暑期课堂报名中！',
        'GPG Sign Party &amp; Yubikey安利',
        'SJTUG开学聚餐']

@app.route('/', methods=['GET', 'POST'])
def home():
    return render_template('home.html', heading1='SJTUG', heading2='SJTU *NIX User Group', posts=posts)

@app.route('/about', methods=['GET', 'POST'])
def about():
    return render_template('about.html', heading1='SJTUG', heading2='SJTU *NIX User Group')

@app.route('/addpost', methods=['GET'])
def add_post_form():
    return render_template('addpost.html', heading1='Admin', heading2='Add a new post')

@app.route('/addpost', methods=['POST'])
def add_post():
    new_post = request.form['posttitle']
    if new_post != None:
        posts.append(new_post)
        return render_template('info.html', heading1='Success', heading2='Successfully added a new post')
    else:
        return render_template('info.html', heading1='Failed', heading2='Please input the post title')

@app.errorhandler(404)
def page_not_found(error):
    return render_template('info.html', heading1='404 Error', heading2='Page Not Found'), 404

if __name__ == '__main__':
    app.run()