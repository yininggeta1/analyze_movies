import sys

def analyze(group, out = 20):
    ##the function analyze takes two arguments:
    ##  1. group: the grouping method, either by gender, or by age group
    ##  2. out: the number of top rating movies to be printed out, default value is 20, to avoid lengthy output
    ##this function is used to read raw datasets from three files, namely users.dat, movies.dat and ratings.dat
    ##and compute average rating for each movie, and print the top rating movies for each group.
    ##please ensure that this script is placed in the same directory as raw datasets before running on command
    ##line.
    
    #ensuring input argument type is correct
    group = str(group)
    out = int(out)

    ##read data from three files and split using the '::' delimiter
    
    #read movies.dat
    movies = open('movies.dat', 'r')
    movies_list = list()
    movies_ind = list() #full list for movie index, as there is some missing indices in movies.dat
    for movie in movies.readlines():
     movies_list.append(movie.strip().split('::'))
     movies_ind.append(int(movie.strip().split('::')[0])) #mapping each row to movie index

    #read users.dat
    users = open('users.dat', 'r')
    users_list = list()
    for user in users.readlines():
        users_list.append(user.strip().split('::'))

    #read ratings.dat
    ratings = open('ratings.dat', 'r')
    ratings_list = list()
    for rating in ratings.readlines():
        ratings_list.append(rating.strip().split('::'))

    ##group by gender
    #as there are only two gender groups, the same task is repeated for both female and male users
    if group == 'gender' or group == 'sex':

        #for each gender, pre-allocate lists of total score and rating counts by movies
        F_total = [0] * 3952
        F_count = [0] * 3952
        M_total = [0] * 3952
        M_count = [0] * 3952

        #loop through the 1 million ratings and add rating score
        for i_rating in range(len(ratings_list)):
            user_ind = int(ratings_list[i_rating][0])
            movie_ind = int(ratings_list[i_rating][1])
            gender = users_list[user_ind-1][1]
            if gender == 'F':
                F_total[movie_ind-1] += int(ratings_list[i_rating][2])
                F_count[movie_ind-1] += 1
            elif gender == 'M':
                M_total[movie_ind-1] += int(ratings_list[i_rating][2])
                M_count[movie_ind-1] += 1

        #calculate average rating score, which is total score divided by its count of ratings
        F_score = [0] * 3952
        M_score = [0] * 3952
        for score in range(3952):
            if F_count[score] != 0:
                F_score[score] = F_total[score]/float(F_count[score])
            if M_count[score] != 0:
                M_score[score] = M_total[score]/float(M_count[score])

        #match average ratings with movie indices, and sort in descending order according to average rating
        F_zip = zip(F_score, range(1,3953))
        F_top = sorted(F_zip, reverse = True)
        M_zip = zip(M_score, range(1,3953))
        M_top = sorted(M_zip, reverse = True)

        #print out top movies for each gender
        print "Top", out, "movies rated by female"
        for ftop in range(out):
            movie_ind = F_top[ftop][1]
            print movies_list[movies_ind.index(movie_ind)-1][1], "*", round(F_top[ftop][0], 1)
        print "-------------------------------------------------"
        print "Top", out, "movies rate by male, average rating"
        for mtop in range(out):
            movie_ind = M_top[mtop][1]
            print movies_list[movies_ind.index(movie_ind)-1][1], "*", round(M_top[mtop][0], 1)

    ##group by age group
    #as there are seven age groups, in order to keep the code short, a for loop is used to sequentially go
    #through each age group and print its top movies
    elif group == 'agegroup' or group == 'age' or group == 'age group':

        #match age groups with its labels
        n_age = [1, 18, 25, 35, 45, 50, 56]
        n_age_label = ["Under 18", "18-24", "25-34", "35-44", "45-49", "50-55", "56+"]
        n_age_zip = zip(n_age, n_age_label)
        
        for i_age in n_age_zip:
            age = i_age[0]
            age_label = i_age[1]

            #pre-allocate lists of total score and rating counts by movies
            age_total = [0] * 3952
            age_count = [0] * 3952

            #loop through the 1 million ratings and add rating score
            for i_rating in range(len(ratings_list)):
                user_ind = int(ratings_list[i_rating][0])
                movie_ind = int(ratings_list[i_rating][1])
                age_group = int(users_list[user_ind-1][2])
                if age_group == age:
                    age_total[movie_ind-1] += int(ratings_list[i_rating][2])
                    age_count[movie_ind-1] += 1

            #calculate average rating score, which is total score divided by its count of ratings
            age_score = [0] * 3952
            for score in range(3952):
                if age_count[score] != 0:
                    age_score[score] = age_total[score]/float(age_count[score])

            #match average ratings with movie indices, and sort in descending order according to average rating
            age_zip = zip(age_score, range(1, 3953))
            age_top = sorted(age_zip, reverse = True)

            #print out top movies for each age group
            print "-------------------------------------------------"
            print "Top", out, "movies rated by age group: ", age_label
            for top in range(out):
                movie_ind = age_top[top][1]
                print movies_list[movies_ind.index(movie_ind)-1][1], "*", round(age_top[top][0], 1)
    else:
        #warning message
        print "Please select either gender or agegroup as grouping method"


if __name__ == '__main__':
    analyze(*sys.argv[1:])
