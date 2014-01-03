
evid = {}
for l in open('evidence'):
	(id, value) = l.rstrip().split('\t')
	evid[id] = value

label = {}
for l in open('label'):
	(id, value) = l.rstrip().split('\t')
	label[id] = value


vids = {}
fids = {}
fo = open("variables.txt", 'w')
ff = open("factors.txt", 'w')
for edge in open('network'):

	(n1, n2) = edge.rstrip().split(' ')

	(l1, id1) = n1[1:].split('-')
	(l2, id2) = n2[1:].split('-')

	sig1 = l1 + "-" + id1
	sig2 = l2 + "-" + id2

	if sig1 not in vids:
		vids[sig1] = len(vids)
	if sig2 not in vids:
		vids[sig2] = len(vids)


	for (sig, l, id) in [(sig1, l1, id1), (sig2, l2, id2)]:
		isEvid = False
		value = 0
		if l == "0" and id in evid:
			isEvid = True
			value = evid[id]
		if l == "5" and id in label:
			isEvid = True
			value = label[id]
		#print isEvid, value

		fid = 0
		if sig == sig1:
			if sig1 not in fids:
				fids[sig1] = len(fids)
				fid = fids[sig1]
				if l == "5":
					ff.write("%d\t0\tSOFTMAX\n" % (fid))
				else:
					ff.write("%d\t0\tSUMTANH\n" % (fid))
				fo.write("%d\t%d\t0\ttrue\tDiscrete\t%s\t%s\t%s\n" % (vids[sig], fid, value, isEvid.__repr__().lower(), (not isEvid).__repr__().lower()))
		else:
			fid = fids[sig1]
			fo.write("%d\t%d\t1\ttrue\tDiscrete\t%s\t%s\t%s\n" % (vids[sig], fid, value, isEvid.__repr__().lower(), (not isEvid).__repr__().lower()))


fw = open("weights.txt", "w")
fw.write("0\t0.0\tfalse\tNNWEIGHTS\n")

fw.close()
fo.close()
ff.close()
	#print id1, id2
	




